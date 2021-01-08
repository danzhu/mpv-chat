module Network.Mpv
  ( Mpv,
    MpvError (..),
    command,
    eventChan,
    getProperty,
    loadfile,
    newMpvClient,
    observeEvent,
    observeProperty,
    runMpv,
    setProperty,
  )
where

import Control.Concurrent.Task (withTask_)
import Control.Monad.Catch
  ( MonadThrow,
    throwM,
  )
import Control.Monad.RWS (ask)
import Data.Aeson
  ( Value (Null),
    eitherDecodeStrict',
    encode,
    object,
    parseJSON,
    toJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Network.Unix
  ( AppDataUnix,
    appSink,
    appSource,
    clientSettings,
    runUnixClient,
  )
import qualified Data.IdMap as IM

type Prop = Text

type Error = Text

data Request = ReqCommand
  { reqId :: IM.Id,
    reqAsync :: Bool,
    reqCommand :: [Value]
  }
  deriving stock (Show)

instance ToJSON Request where
  toJSON (ReqCommand rid asy cmd) =
    object
      [ "request_id" .= rid,
        "async" .= asy,
        "command" .= cmd
      ]

data Event
  = EvtPropChange IM.Id Prop Value
  | EvtOther Text
  deriving stock (Show)

-- 'Maybe' needed as 'data' field may not exist
type Data = Maybe Value

type Reply = Either Error Data

data Response
  = ResEvent Event
  | ResReply IM.Id Reply
  deriving stock (Show)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    let evt "property-change" =
          EvtPropChange
            <$> o .: "id"
            <*> o .: "name"
            <*> o .:? "data" .!= Null
        evt name = pure $ EvtOther name
        rep "success" = Right <$> o .:? "data"
        rep err = pure $ Left err
        event = ResEvent <$> (evt =<< o .: "event")
        reply = ResReply <$> (o .: "request_id") <*> (rep =<< o .: "error")
    event <|> reply

data MpvError
  = MpvIpcError Error
  | MpvTypeError String
  | MpvJsonError ByteString String
  | MpvNoReqId
  deriving stock (Show)
  deriving anyclass (Exception)

data Mpv = Mpv
  { requests :: TBQueue ([Value], TMVar Reply),
    waits :: TVar (IM.IdMap (TMVar Reply)),
    handlers :: TVar (HashMap Text (IO ())),
    changes :: TVar (IM.IdMap (Value -> IO ())),
    events :: TChan Event
  }

type MpvIO = ReaderT Mpv IO

expect :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
expect e = maybe (throwM e) pure

expectE :: (MonadThrow m, Exception e) => (b -> e) -> Either b a -> m a
expectE e = either (throwM . e) pure

newMpvClient :: MonadIO m => m Mpv
newMpvClient =
  Mpv
    <$> newTBQueueIO 16
    <*> newTVarIO (IM.empty 1)
    <*> newTVarIO mempty
    <*> newTVarIO (IM.empty 1)
    <*> newTChanIO

produce :: MpvIO ByteString
produce = do
  mpv <- ask
  (cmd, wait) <- atomically $ readTBQueue $ requests mpv
  rid <- atomically $ stateTVar (waits mpv) $ IM.insert wait
  let line = toStrict $ encode $ ReqCommand rid True cmd
  liftIO $ BC.putStrLn $ "> " <> line
  pure line

consume :: ByteString -> MpvIO ()
consume line = do
  liftIO $ BC.putStrLn $ "  " <> line
  mpv <- ask
  res <- expectE (MpvJsonError line) $ eitherDecodeStrict' line
  case res of
    ResEvent evt -> atomically $ writeTChan (events mpv) evt
    ResReply rid rep -> atomically $ do
      v <- stateTVar (waits mpv) $ IM.remove rid
      w <- expect MpvNoReqId v
      putTMVar w rep

stdin :: AppDataUnix -> MpvIO ()
stdin app =
  runConduit $
    C.repeatM produce
      .| C.unlinesAscii
      .| appSink app

stdout :: AppDataUnix -> MpvIO ()
stdout app =
  runConduit $
    appSource app
      .| C.linesUnboundedAscii
      .| C.mapM_ consume

handle :: MpvIO a
handle = do
  mpv <- ask
  () <- liftIO $ command "disable_event" mpv ("all" :: Text)
  forever $
    atomically (readTChan $ events mpv) >>= \case
      EvtPropChange i _ v -> do
        h <- IM.lookup i <$> readTVarIO (changes mpv)
        liftIO $ traverse_ ($ v) h
      EvtOther e -> do
        h <- lookup e <$> readTVarIO (handlers mpv)
        liftIO $ sequence_ h

runMpv :: MonadIO m => Mpv -> FilePath -> m ()
runMpv mpv ipcPath = liftIO $
  runUnixClient (clientSettings ipcPath) $ \app ->
    flip runReaderT mpv $
      withTask_ (stdin app `concurrently_` handle) $
        stdout app

fromJson :: FromJSON a => Value -> IO a
fromJson = expectE MpvTypeError . parseEither parseJSON

eventChan :: MonadIO m => Mpv -> m (TChan Event)
eventChan mpv = atomically $ dupTChan $ events mpv

observeEvent :: MonadUnliftIO m => Mpv -> Text -> m () -> m ()
observeEvent mpv e f = withRunInIO $ \run -> do
  atomically $ modifyTVar' (handlers mpv) $ insertWith (<>) e $ run f
  command "enable_event" mpv e

class CommandType a where
  commandValue :: [Value] -> Mpv -> a

instance FromJSON r => CommandType (IO r) where
  commandValue args mpv = do
    wait <- newEmptyTMVarIO
    atomically $ writeTBQueue (requests mpv) (reverse args, wait)
    reply <- atomically $ readTMVar wait
    r <- expectE MpvIpcError reply
    -- HACK: round-trip converting '()' to 'Value' and back,
    -- to reuse 'FromJSON' and avoid custom constraints
    fromJson $ fromMaybe (toJSON ()) r

instance (ToJSON a, CommandType c) => CommandType (a -> c) where
  commandValue args mpv a = commandValue (toJSON a : args) mpv

command :: CommandType c => Text -> Mpv -> c
command cmd = commandValue [toJSON cmd]

newtype LoadData = LoadData
  { playlist_entry_id :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

loadfile :: Mpv -> Text -> IO LoadData
loadfile = command "loadfile"

getProperty :: FromJSON a => Mpv -> Prop -> IO a
getProperty = command "get_property"

setProperty :: Mpv -> Prop -> Value -> IO ()
setProperty = command "set_property"

observeProperty ::
  (MonadUnliftIO m, FromJSON a) =>
  Mpv ->
  Prop ->
  (a -> m ()) ->
  m ()
observeProperty mpv p f = withRunInIO $ \run -> do
  i <- atomically $ stateTVar (changes mpv) $ IM.insert $ run . f <=< fromJson
  command "observe_property" mpv i p
