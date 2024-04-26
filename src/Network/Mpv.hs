module Network.Mpv
  ( -- * Basic Operation
    newMpvClient,
    runMpv,
    command,
    observeEvent,
    observeProperty,

    -- * Types
    Mpv,
    MpvError (..),
    CommandName (..),
    PropertyName (..),
    EventName (..),
    Error,
    CommandType (commandValue),

    -- * Commands
    LoadData,
    loadfile,
    getProperty,
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
    ToJSON (toJSON),
    Value (Null),
    eitherDecodeStrict',
    encode,
    parseJSON,
    toJSON,
    withObject,
    (.!=),
    (.:),
    (.:?),
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

newtype CommandName = CommandName Text
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, ToJSON, IsString)

newtype PropertyName = PropertyName Text
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, ToJSON, IsString)

newtype EventName = EventName Text
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, ToJSON, IsString)

type Error = Text

type Command = [Value]

data Request = ReqCommand
  { request_id :: IM.Id,
    async :: Bool,
    command :: Command
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Event
  = EvtPropChange IM.Id PropertyName Value
  | EvtOther EventName
  deriving stock (Show)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o ->
    o .: "event" >>= \case
      "property-change" ->
        EvtPropChange
          <$> o .: "id"
          <*> o .: "name"
          <*> o .:? "data" .!= Null
      name -> pure $ EvtOther name

type Reply = Either Error Value

data Response
  = ResEvent Event
  | ResReply IM.Id Reply
  deriving stock (Show)

instance FromJSON Response where
  parseJSON v =
    ResEvent <$> parseJSON v
      <|> parseReply v
    where
      parseReply = withObject "Response" $ \o -> do
        let rep =
              o .: "error" >>= \case
                "success" -> Right <$> o .:? "data" .!= Null
                err -> pure $ Left err
        ResReply <$> (o .: "request_id") <*> rep

data MpvError
  = MpvIpcError Error
  | MpvTypeError String
  | MpvJsonError ByteString String
  | MpvNoReqId
  deriving stock (Show)
  deriving anyclass (Exception)

data Mpv = Mpv
  { requests :: TBQueue (Command, TMVar Reply),
    waits :: TVar (IM.IdMap (TMVar Reply)),
    handlers :: TVar (HashMap EventName (IO ())),
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
  let req = ReqCommand {request_id = rid, async = True, command = cmd}
      line = toStrict $ encode req
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

class CommandType a where
  commandValue :: Mpv -> [Value] -> a

instance FromJSON r => CommandType (IO r) where
  commandValue Mpv {requests} args = do
    wait <- newEmptyTMVarIO
    atomically $ writeTBQueue requests (reverse args, wait)
    reply <- atomically $ readTMVar wait
    r <- expectE MpvIpcError reply
    fromJson r

instance (ToJSON a, CommandType c) => CommandType (a -> c) where
  commandValue mpv args a = commandValue mpv $ toJSON a : args

command :: CommandType c => Mpv -> CommandName -> c
command mpv = commandValue mpv []

observeEvent :: MonadUnliftIO m => Mpv -> EventName -> m () -> m ()
observeEvent mpv@Mpv {handlers} e f = withRunInIO $ \run -> do
  atomically $ modifyTVar' handlers $ insertWith (<>) e $ run f
  command mpv "enable_event" e

observeProperty ::
  (MonadUnliftIO m, FromJSON a) =>
  Mpv ->
  PropertyName ->
  (a -> m ()) ->
  m ()
observeProperty mpv@Mpv {changes} p f = withRunInIO $ \run -> do
  i <- atomically $ stateTVar changes $ IM.insert $ run . f <=< fromJson
  command mpv "observe_property" i p

newtype LoadData = LoadData
  { playlist_entry_id :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

loadfile :: Mpv -> Text -> IO LoadData
loadfile mpv = command mpv "loadfile"

getProperty :: FromJSON a => Mpv -> PropertyName -> IO a
getProperty mpv = command mpv "get_property"

setProperty :: Mpv -> PropertyName -> Value -> IO ()
setProperty mpv = command mpv "set_property"
