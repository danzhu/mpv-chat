module Network.Mpv
  ( Mpv,
    MpvError (..),
    Prop,
    Error,
    Event,
    CommandType (commandValue),
    LoadData,
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
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Null),
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
import Data.Conduit (runConduit, (.|))
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

produce :: Mpv -> IO ByteString
produce Mpv {requests, waits} = do
  (cmd, wait) <- atomically $ readTBQueue requests
  rid <- atomically $ stateTVar waits $ IM.insert wait
  let line = toStrict $ encode $ ReqCommand rid True cmd
  liftIO $ BC.putStrLn $ "> " <> line
  pure line

consume :: Mpv -> ByteString -> IO ()
consume Mpv {events, waits} line = do
  liftIO $ BC.putStrLn $ "  " <> line
  res <- expectE (MpvJsonError line) $ eitherDecodeStrict' line
  case res of
    ResEvent evt -> atomically $ writeTChan events evt
    ResReply rid rep -> atomically $ do
      v <- stateTVar waits $ IM.remove rid
      w <- expect MpvNoReqId v
      putTMVar w rep

stdin :: Mpv -> AppDataUnix -> IO ()
stdin mpv app =
  runConduit $
    C.repeatM (produce mpv)
      .| C.unlinesAscii
      .| appSink app

stdout :: Mpv -> AppDataUnix -> IO ()
stdout mpv app =
  runConduit $
    appSource app
      .| C.linesUnboundedAscii
      .| C.mapM_ (consume mpv)

handle :: Mpv -> IO a
handle mpv@Mpv {changes, handlers, events} = do
  () <- liftIO $ command mpv "disable_event" ("all" :: Text)
  forever $
    atomically (readTChan events) >>= \case
      EvtPropChange i _ v -> do
        h <- IM.lookup i <$> readTVarIO changes
        liftIO $ traverse_ ($ v) h
      EvtOther e -> do
        h <- lookup e <$> readTVarIO handlers
        liftIO $ sequence_ h

runMpv :: MonadIO m => Mpv -> FilePath -> m ()
runMpv mpv ipcPath = liftIO $
  runUnixClient (clientSettings ipcPath) $ \app ->
    withTask_ (stdin mpv app `concurrently_` handle mpv) $
      stdout mpv app

fromJson :: FromJSON a => Value -> IO a
fromJson = expectE MpvTypeError . parseEither parseJSON

eventChan :: MonadIO m => Mpv -> m (TChan Event)
eventChan Mpv {events} = atomically $ dupTChan events

observeEvent :: MonadUnliftIO m => Mpv -> Text -> m () -> m ()
observeEvent mpv@Mpv {handlers} e f = withRunInIO $ \run -> do
  atomically $ modifyTVar' handlers $ insertWith (<>) e $ run f
  command mpv "enable_event" e

class CommandType a where
  commandValue :: Mpv -> [Value] -> a

instance FromJSON r => CommandType (IO r) where
  commandValue Mpv {requests} args = do
    wait <- newEmptyTMVarIO
    atomically $ writeTBQueue requests (reverse args, wait)
    reply <- atomically $ readTMVar wait
    r <- expectE MpvIpcError reply
    -- HACK: round-trip converting '()' to 'Value' and back,
    -- to reuse 'FromJSON' and avoid custom constraints
    fromJson $ fromMaybe (toJSON ()) r

instance (ToJSON a, CommandType c) => CommandType (a -> c) where
  commandValue mpv args a = commandValue mpv $ toJSON a : args

command :: CommandType c => Mpv -> Text -> c
command mpv = commandValue mpv []

newtype LoadData = LoadData
  { playlist_entry_id :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

loadfile :: Mpv -> Text -> IO LoadData
loadfile mpv = command mpv "loadfile"

getProperty :: FromJSON a => Mpv -> Prop -> IO a
getProperty mpv = command mpv "get_property"

setProperty :: Mpv -> Prop -> Value -> IO ()
setProperty mpv = command mpv "set_property"

observeProperty ::
  (MonadUnliftIO m, FromJSON a) =>
  Mpv ->
  Prop ->
  (a -> m ()) ->
  m ()
observeProperty mpv@Mpv {changes} p f = withRunInIO $ \run -> do
  i <- atomically $ stateTVar changes $ IM.insert $ run . f <=< fromJson
  command mpv "observe_property" i p
