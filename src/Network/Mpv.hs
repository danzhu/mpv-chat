{-# LANGUAGE NumericUnderscores #-}

module Network.Mpv
  ( MpvError(..)
  , command
  , eventChan
  , getProperty
  , loadfile
  , newMpvClient
  , observeEvent
  , observeProperty
  , runMpv
  , setProperty
  ) where

import           Control.Concurrent.Task        ( withTask_ )
import           Control.Monad.ContT            ( contT_ )
import qualified Data.IdMap                    as IM

import           Control.Applicative            ( (<|>) )
import           Control.Concurrent.Async       ( concurrently_ )
import           Control.Concurrent.STM         ( atomically )
import           Control.Concurrent.STM.TBQueue ( TBQueue
                                                , newTBQueueIO
                                                , readTBQueue
                                                , writeTBQueue
                                                )
import           Control.Concurrent.STM.TChan   ( TChan
                                                , dupTChan
                                                , newTChanIO
                                                , readTChan
                                                , writeTChan
                                                )
import           Control.Concurrent.STM.TMVar   ( TMVar
                                                , newEmptyTMVarIO
                                                , putTMVar
                                                , readTMVar
                                                )
import           Control.Concurrent.STM.TVar    ( TVar
                                                , modifyTVar'
                                                , newTVarIO
                                                , readTVarIO
                                                , stateTVar
                                                )
import           Control.Exception              ( Exception )
import           Control.Monad                  ( forever
                                                , (<=<)
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Cont       ( ContT(ContT)
                                                , evalContT
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , Value
                                                , eitherDecodeStrict'
                                                , encode
                                                , object
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import           Data.Aeson.Types               ( parseEither )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Data.Conduit                   ( runConduit
                                                , (.|)
                                                )
import qualified Data.Conduit.Combinators      as C
import           Data.Conduit.Network.Unix      ( AppDataUnix
                                                , appSink
                                                , appSource
                                                , clientSettings
                                                , runUnixClient
                                                )
import           Data.Foldable                  ( sequenceA_
                                                , traverse_
                                                )
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T

type Prop = T.Text
type Error = T.Text

data Request
  = ReqCommand IM.Id Value
  deriving (Show)

instance ToJSON Request where
  toJSON (ReqCommand rid cmd) = object ["request_id" .= rid, "command" .= cmd]

data Event
  = EvtPropChange IM.Id Prop Value
  | EvtOther T.Text
  deriving (Show)

-- 'Maybe' needed as 'data' field may not exist
type Data = Maybe Value

type Reply = Either Error Data

data Response
  = ResEvent Event
  | ResReply IM.Id Reply
  deriving (Show)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    let evt "property-change" = EvtPropChange
          <$> o .: "id"
          <*> o .: "name"
          <*> o .: "data"
        evt name = pure $ EvtOther name
        rep "success" = Right <$> o .:? "data"
        rep err       = pure $ Left err
        event = ResEvent <$> (evt =<< o .: "event")
        reply = ResReply <$> (o .: "request_id") <*> (rep =<< o .: "error")
    event <|> reply

data MpvError
  = MpvIpcError Error
  | MpvTypeError String
  | MpvJsonError B.ByteString String
  | MpvNoReqId
  deriving (Show)

instance Exception MpvError

data Mpv = Mpv
  { requests :: TBQueue ([Value], TMVar Reply)
  , waits :: TVar (IM.IdMap (TMVar Reply))
  , handlers :: TVar (HM.HashMap T.Text (IO ()))
  , changes :: TVar (IM.IdMap (Value -> IO ()))
  , events :: TChan Event
  }

newMpvClient :: IO Mpv
newMpvClient = Mpv
  <$> newTBQueueIO 16
  <*> newTVarIO (IM.empty 1)
  <*> newTVarIO HM.empty
  <*> newTVarIO (IM.empty 1)
  <*> newTChanIO

expect :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
expect e = maybe (throwM e) pure

expectE :: (MonadThrow m, Exception e) => (b -> e) -> Either b a -> m a
expectE e = either (throwM . e) pure

produce :: Mpv -> IO B.ByteString
produce mpv = do
  (cmd, wait) <- atomically $ readTBQueue $ requests mpv
  rid <- atomically $ stateTVar (waits mpv) $ IM.insert wait
  pure $ LB.toStrict $ encode $ ReqCommand rid $ toJSON cmd

consume :: Mpv -> B.ByteString -> IO ()
consume mpv line = do
  res <- expectE (MpvJsonError line) $ eitherDecodeStrict' line
  case res of
    ResEvent evt -> atomically $ writeTChan (events mpv) evt
    ResReply rid rep -> atomically $ do
      v <- stateTVar (waits mpv) $ IM.remove rid
      w <- expect MpvNoReqId v
      putTMVar w rep

stdin :: Mpv -> AppDataUnix -> IO ()
stdin mpv app = runConduit $
  C.repeatM (produce mpv)
  .| C.unlinesAscii
  .| appSink app

stdout :: Mpv -> AppDataUnix -> IO ()
stdout mpv app = runConduit $
  appSource app
  .| C.linesUnboundedAscii
  .| C.mapM_ (consume mpv)

handle :: Mpv -> IO a
handle mpv = forever $ atomically (readTChan $ events mpv) >>= \case
  EvtPropChange i _ v -> do
    h <- IM.lookup i <$> readTVarIO (changes mpv)
    traverse_ ($ v) h
  EvtOther e -> do
    h <- HM.lookup e <$> readTVarIO (handlers mpv)
    sequenceA_ h

runMpv :: Mpv -> FilePath -> IO ()
runMpv mpv ipcPath = evalContT $ do
  app <- ContT $ runUnixClient $ clientSettings ipcPath
  contT_ $ withTask_ $ stdin mpv app `concurrently_` handle mpv
  liftIO $ stdout mpv app

fromJson :: FromJSON a => Value -> IO a
fromJson = expectE MpvTypeError . parseEither parseJSON

eventChan :: Mpv -> IO (TChan Event)
eventChan mpv = atomically $ dupTChan $ events mpv

observeEvent :: Mpv -> T.Text -> IO () -> IO ()
observeEvent mpv e f = atomically $
  modifyTVar' (handlers mpv) $ HM.insertWith (<>) e f

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

command :: CommandType c => T.Text -> Mpv -> c
command cmd = commandValue [toJSON cmd]

loadfile :: Mpv -> T.Text -> IO ()
loadfile = command "loadfile"

getProperty :: FromJSON a => Mpv -> Prop -> IO a
getProperty = command "get_property"

setProperty :: Mpv -> Prop -> Value -> IO ()
setProperty = command "set_property"

observeProperty :: FromJSON a => Mpv -> Prop -> (a -> IO ()) -> IO ()
observeProperty mpv p f = do
  i <- atomically $ stateTVar (changes mpv) $ IM.insert $ f <=< fromJson
  command "observe_property" mpv i p
