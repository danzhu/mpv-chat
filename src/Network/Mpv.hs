{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Mpv
  ( Event(EvtPropChange, EvtOther)
  , command
  , command'
  , eventChan
  , getProperty
  , loadfile
  , observeEvent
  , observeProperty
  , setProperty
  , tryGetProperty
  , trySetProperty
  , withMpv
  ) where

import qualified Data.IdMap                    as IM

import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently_
                                                , link
                                                , withAsync
                                                )
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
import           Control.Exception              ( Exception
                                                , bracket
                                                , tryJust
                                                )
import           Control.Monad                  ( forever
                                                , guard
                                                , when
                                                , (<=<)
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Cont       ( ContT(ContT)
                                                , runContT
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
import           Data.Conduit.Network           ( sinkSocket
                                                , sourceSocket
                                                )
import           Data.Either                    ( isLeft )
import           Data.Foldable                  ( sequenceA_
                                                , traverse_
                                                )
import           Data.Functor                   ( void )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Network.Socket                 ( Family(AF_UNIX)
                                                , SockAddr(SockAddrUnix)
                                                , Socket
                                                , SocketType(Stream)
                                                , close
                                                , connect
                                                , defaultProtocol
                                                , socket
                                                )
import           System.IO.Error                ( isDoesNotExistError )

type Id = Int
type Prop = T.Text
type Error = T.Text

data Mpv = Mpv
  { requests :: TBQueue (Value, TMVar Reply)
  , handlers :: TVar (HM.HashMap T.Text [IO ()])
  , changes :: TVar (IM.IdMap (Value -> IO ()))
  , events :: TChan Event
  }

data MpvError
  = MpvError Error
  | MpvTypeError String
  | MpvJsonError B.ByteString String
  | MpvNoReqId
  | MpvNoData
  deriving (Show)

data Request
  = ReqCommand Id Value
  deriving (Show)

data Event
  = EvtPropChange Id Prop Value
  | EvtOther T.Text
  deriving (Show)

type Data = Maybe Value

type Reply = Either Error Data

data Response
  = ResEvent Event
  | ResReply Id Reply
  deriving (Show)

instance Exception MpvError

instance ToJSON Request where
  toJSON (ReqCommand rid cmd) = object ["request_id" .= rid, "command" .= cmd]

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

expect :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
expect e = maybe (throwM e) pure

expectE :: (MonadThrow m, Exception e) => (b -> e) -> Either b a -> m a
expectE e = either (throwM . e) pure

withSocket :: String -> (Socket -> IO a) -> IO a
withSocket ipcPath f = bracket open close use where
  open = socket AF_UNIX Stream defaultProtocol
  use sock = conn sock *> f sock
  conn sock = do
    let redo = guard . isDoesNotExistError
    r <- tryJust redo $ connect sock $ SockAddrUnix ipcPath
    when (isLeft r) $ threadDelay 100_000 *> conn sock

withMpv :: FilePath -> (Mpv -> IO a) -> IO a
withMpv ipcPath f = do
  waits <- newTVarIO $ IM.empty 1
  reqs <- newTBQueueIO 16
  hans <- newTVarIO HM.empty
  chns <- newTVarIO $ IM.empty 1
  evts <- newTChanIO
  let produce = do
        (cmd, wait) <- atomically $ readTBQueue reqs
        rid <- atomically $ stateTVar waits $ IM.insert wait
        pure $ LB.toStrict $ encode $ ReqCommand rid cmd
      consume line = do
        res <- expectE (MpvJsonError line) $ eitherDecodeStrict' line
        case res of
          ResEvent evt -> atomically $ writeTChan evts evt
          ResReply rid rep -> atomically $ do
            v <- stateTVar waits $ IM.remove rid
            w <- expect MpvNoReqId v
            putTMVar w rep
      handle = forever $ atomically (readTChan evts) >>= \case
        EvtPropChange i _ v -> do
          h <- IM.lookup i <$> readTVarIO chns
          traverse_ ($ v) h
        EvtOther e -> do
          h <- HM.lookup e <$> readTVarIO hans
          traverse_ sequenceA_ h
      mpv = Mpv
        { requests = reqs
        , handlers = hans
        , changes = chns
        , events = evts
        }
  flip runContT pure $ do
    sock <- ContT $ withSocket ipcPath
    let stdin = runConduit $
          C.repeatM produce
          .| C.unlinesAscii
          .| sinkSocket sock
        stdout = runConduit $
          sourceSocket sock
          .| C.linesUnboundedAscii
          .| C.mapM_ consume
    a <- ContT $ withAsync $ stdin `concurrently_` stdout `concurrently_` handle
    liftIO $ link a
    liftIO $ f mpv

fromJson :: FromJSON a => Value -> IO a
fromJson = expectE MpvTypeError . parseEither parseJSON

getData :: FromJSON a => Data -> IO a
getData = fromJson <=< expect MpvNoData

eventChan :: Mpv -> IO (TChan Event)
eventChan mpv = atomically $ dupTChan $ events mpv

observeEvent :: Mpv -> T.Text -> IO () -> IO ()
observeEvent mpv e f = atomically $
  modifyTVar' (handlers mpv) $ HM.insertWith (<>) e [f]

command :: ToJSON cmd => Mpv -> cmd -> IO Reply
command mpv cmd = do
  wait <- newEmptyTMVarIO
  atomically $ writeTBQueue (requests mpv) (toJSON cmd, wait)
  atomically $ readTMVar wait

command' :: ToJSON cmd => Mpv -> cmd -> IO Data
command' mpv = expectE MpvError <=< command mpv

cLoadfile :: T.Text
cLoadfile = "loadfile"

cGetProperty :: T.Text
cGetProperty = "get_property"

cSetProperty :: T.Text
cSetProperty = "set_property"

cObserveProperty :: T.Text
cObserveProperty = "observe_property"

loadfile :: Mpv -> T.Text -> IO ()
loadfile mpv url = void $ command' mpv (cLoadfile, url)

tryGetProperty :: FromJSON a => Mpv -> Prop -> IO (Either Error a)
tryGetProperty mpv p = traverse getData =<< command mpv (cGetProperty, p)

getProperty :: FromJSON a => Mpv -> Prop -> IO a
getProperty mpv p = getData =<< command' mpv (cGetProperty, p)

trySetProperty :: Mpv -> Prop -> Value -> IO (Either Error ())
trySetProperty mpv p v = void <$> command mpv (cSetProperty, p, v)

setProperty :: Mpv -> Prop -> Value -> IO ()
setProperty mpv p v = void $ command' mpv (cSetProperty, p, v)

observeProperty :: FromJSON a => Mpv -> Prop -> (a -> IO ()) -> IO ()
observeProperty mpv p f = do
  i <- atomically $ stateTVar (changes mpv) $ IM.insert (f <=< fromJson)
  void $ command' mpv (cObserveProperty, i, p)
