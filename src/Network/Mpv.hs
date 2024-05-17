module Network.Mpv
  ( -- * Basic Operation
    withMpv,
    waitMpv,
    command,
    command',
    observeEvent_,
    observeProperty,

    -- * Types
    Mpv,
    MpvError (..),
    MpvProtocolError (..),
    CommandName (..),
    PropertyName (..),
    EventName (..),
    Command,
    Error,
    MpvProperty,
    CommandType (commandValue),

    -- * Commands
    clientName,
    LoadData (..),
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
import Data.Time (NominalDiffTime)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Newtype for mpv command names, e.g. "loadfile".
newtype CommandName = CommandName Text
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, ToJSON, IsString)

-- | Newtype for mpv event names, e.g. "seek".
newtype EventName = EventName Text
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, ToJSON, IsString)

-- | Type for errors returned from mpv (string).
type Error = Text

-- | Type for commands sent to mpv.
-- TODO: support JSON object (named arguments).
type Command = [Value]

data Request = ReqCommand
  { command :: Command,
    request_id :: IM.Id,
    async :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Event
  = EvtPropChange IM.Id Text Value
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
  parseJSON v = ResEvent <$> parseJSON v <|> parseReply v
    where
      parseReply = withObject "Response" $ \o -> do
        let rep =
              o .: "error" >>= \case
                "success" -> Right <$> o .:? "data" .!= Null
                err -> pure $ Left err
        ResReply <$> (o .: "request_id") <*> rep

-- | Errors returned by mpv.
newtype MpvError = MpvIpcError Error
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Programming errors, indicating bugs/misuse.
data MpvProtocolError
  = MpvTypeError String
  | MpvJsonError ByteString String
  | MpvNoReqId
  deriving stock (Show)
  deriving anyclass (Exception)

-- | IPC connection state.
data Mpv = Mpv
  { requests :: TBQueue (Command, TMVar Reply),
    waits :: TVar (IM.IdMap (TMVar Reply)),
    handlers :: TVar (HashMap EventName (IO ())),
    changes :: TVar (IM.IdMap (Value -> IO ())),
    events :: TChan Event,
    closed :: TVar Bool
  }

expect :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
expect e = maybe (throwM e) pure

expectE :: (MonadThrow m, Exception e) => (b -> e) -> Either b a -> m a
expectE e = either (throwM . e) pure

newMpvClient :: (MonadIO m) => m Mpv
newMpvClient =
  Mpv
    <$> newTBQueueIO 16
    <*> newTVarIO (IM.empty 1)
    <*> newTVarIO mempty
    <*> newTVarIO (IM.empty 1)
    <*> newTChanIO
    <*> newTVarIO False

produce :: Mpv -> IO Request
produce Mpv {requests, waits} = do
  (cmd, wait) <- atomically $ readTBQueue requests
  rid <- atomically $ stateTVar waits $ IM.insert wait
  pure $ ReqCommand {command = cmd, request_id = rid, async = True}

consume :: Mpv -> Response -> IO ()
consume Mpv {events} (ResEvent evt) = atomically $ writeTChan events evt
consume Mpv {waits} (ResReply rid rep) = atomically $ do
  v <- stateTVar waits $ IM.remove rid
  w <- expect MpvNoReqId v
  putTMVar w rep

encodeReq :: Request -> IO ByteString
encodeReq req = do
  let line = toStrict $ encode req
  -- TODO: find a logging solution and redirect log somewhere else
  BC.putStrLn $ "> " <> line
  pure line

decodeRes :: ByteString -> IO Response
decodeRes line = do
  BC.putStrLn $ "  " <> line
  expectE (MpvJsonError line) $ eitherDecodeStrict' line

send :: Mpv -> AppDataUnix -> IO ()
send mpv app =
  runConduit $
    C.repeatM (produce mpv)
      .| C.mapM encodeReq
      .| C.unlinesAscii
      .| appSink app

recv :: Mpv -> AppDataUnix -> IO ()
recv mpv app =
  runConduit $
    appSource app
      .| C.linesUnboundedAscii
      .| C.mapM decodeRes
      .| C.mapM_ (consume mpv)

-- handle events in a separate thread to avoid deadlocks when user calls mpv
-- within handlers
handle :: Mpv -> IO a
handle Mpv {changes, handlers, events} =
  forever $
    atomically (readTChan events) >>= \case
      EvtPropChange i _ v -> do
        h <- IM.lookup i <$> readTVarIO changes
        traverse_ ($ v) h
      EvtOther e -> do
        h <- lookup e <$> readTVarIO handlers
        sequence_ h

communicate :: Mpv -> AppDataUnix -> IO ()
communicate mpv@Mpv {closed} app =
  send mpv app
    `concurrently_` (recv mpv app *> setClosed)
    `concurrently_` handle mpv
  where
    setClosed = atomically $ writeTVar closed True

-- | Start mpv IPC, and run the provided action to finish before returning.
-- Use `waitMpv` to make the connection stay open until mpv exits.
withMpv :: (MonadUnliftIO m) => FilePath -> (Mpv -> m ()) -> m ()
withMpv ipcPath f = withRunInIO $ \run -> do
  mpv <- newMpvClient
  runUnixClient (clientSettings ipcPath) $ \app ->
    withTask_ (communicate mpv app) $ do
      () <- command mpv "disable_event" ("all" :: Text)
      run (f mpv)

-- | Wait for mpv to close the connection.
waitMpv :: (MonadIO m) => Mpv -> m ()
waitMpv Mpv {closed} = atomically $ guard =<< readTVar closed

fromJson :: (FromJSON a) => Value -> IO a
fromJson = expectE MpvTypeError . parseEither parseJSON

-- | Send command to mpv and return the result.
-- Take command name/args as a single argument.
command' :: (MonadIO m, FromJSON r) => Mpv -> Command -> m r
command' Mpv {requests} args = liftIO $ do
  wait <- newEmptyTMVarIO
  atomically $ writeTBQueue requests (args, wait)
  reply <- atomically $ readTMVar wait
  r <- expectE MpvIpcError reply
  fromJson r

-- | Typeclass for printf-style `command`.
class CommandType a where
  commandValue :: Mpv -> [Value] -> a

instance (FromJSON r) => CommandType (IO r) where
  commandValue mpv args = command' mpv $ reverse args

instance (ToJSON a, CommandType c) => CommandType (a -> c) where
  commandValue mpv args a = commandValue mpv $ toJSON a : args

-- | Send command to mpv and return the result.
-- Use printf-style command name/args passing.
command :: (CommandType c) => Mpv -> CommandName -> c
command mpv = commandValue mpv []

-- | Run the callback whenever the provided event is received.
observeEvent_ :: (MonadUnliftIO m) => Mpv -> EventName -> m () -> m ()
observeEvent_ mpv@Mpv {handlers} e f = withRunInIO $ \run -> do
  atomically $ modifyTVar' handlers $ insertWith (<>) e $ run f
  command mpv "enable_event" e

-- | Typeclass to associate property type to property name.
class MpvProperty (s :: Symbol) a | s -> a

instance MpvProperty "filename" (Maybe Text)

instance MpvProperty "filename/no-ext" (Maybe Text)

instance MpvProperty "playback-time" NominalDiffTime

instance MpvProperty "pause" Bool

instance MpvProperty "sub-delay" NominalDiffTime

-- | Newtype for mpv property names, e.g. "filename".
newtype PropertyName a = PropertyName Text
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, ToJSON, IsString)

instance (KnownSymbol p, MpvProperty p a) => IsLabel p (PropertyName a) where
  fromLabel = PropertyName $ fromList $ symbolVal (Proxy :: Proxy p)

-- | Run the callback whenever the provided property is changed.
observeProperty ::
  (MonadUnliftIO m, FromJSON a) =>
  Mpv ->
  PropertyName a ->
  (a -> m ()) ->
  m ()
observeProperty mpv@Mpv {changes} p f = withRunInIO $ \run -> do
  i <- atomically $ stateTVar changes $ IM.insert $ run . f <=< fromJson
  command mpv "observe_property" i p

--
-- Commands
--

-- | Response from `loadfile`.
newtype LoadData = LoadData
  { playlist_entry_id :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

loadfile :: Mpv -> Text -> IO LoadData
loadfile mpv = command mpv "loadfile"

clientName :: Mpv -> IO Text
clientName mpv = command mpv "client_name"

getProperty :: (FromJSON a) => Mpv -> PropertyName a -> IO a
getProperty mpv = command mpv "get_property"

setProperty :: (ToJSON a) => Mpv -> PropertyName a -> a -> IO ()
setProperty mpv = command mpv "set_property"
