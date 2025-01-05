module Network.Wai.Monad
  ( Wai,
    WaiApp,
    ErrorResponse (..),
    wai,
    runWai,
    throwWai,
    catchWai,
    requestSource,
    requestBS,
    requestJson,
    responseRedirect,
    responseSource,
  )
where

import Data.Aeson
  ( Value,
    json',
  )
import Data.Conduit (ConduitT, runConduit, yield, (.|))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.Combinators as C
import Network.HTTP.Types.Header
  ( ResponseHeaders,
    hLocation,
  )
import Network.HTTP.Types.Status
  ( Status,
    temporaryRedirect307,
  )
import Network.Wai
  ( Request,
    Response,
    ResponseReceived,
    getRequestBodyChunk,
    responseBuilder,
    responseStream,
  )
import UnliftIO.Exception (throwIO)

type Wai = ReaderT Request (ContT ResponseReceived IO)

type WaiApp = Wai Response

wai :: (Request -> (a -> IO ResponseReceived) -> IO ResponseReceived) -> Wai a
wai app = ReaderT $ ContT . app

runWai :: Wai a -> Request -> (a -> IO ResponseReceived) -> IO ResponseReceived
runWai app = runContT . runReaderT app

data ErrorResponse = ErrorResponse {status :: Status, message :: Text}
  deriving stock (Show)
  deriving anyclass (Exception)

throwWai :: (MonadIO m) => Status -> Text -> m a
throwWai status msg = throwIO $ ErrorResponse status msg

catchWai :: Wai a -> (ErrorResponse -> Wai a) -> Wai a
catchWai app handler = wai \req resp ->
  try (runWai app req resp) >>= \case
    Right r -> pure r
    Left e -> runWai (handler e) req resp

requestSource :: (MonadIO m) => Request -> ConduitT i ByteString m ()
requestSource req = do
  chunk <- liftIO $ getRequestBodyChunk req
  unless (null chunk) $ do
    yield chunk
    requestSource req

requestBS :: (MonadIO m) => Request -> m ByteString
requestBS req = liftIO $ runConduit $ requestSource req .| C.fold

requestJson :: (MonadIO m) => Request -> m Value
requestJson req = liftIO $ runConduit $ requestSource req .| sinkParser json'

responseRedirect :: ByteString -> Response
responseRedirect l = responseBuilder temporaryRedirect307 [(hLocation, l)] ""

responseSource ::
  Status ->
  ResponseHeaders ->
  ConduitT () Builder IO () ->
  Response
responseSource s hs bs = responseStream s hs body
  where
    body write flush = runConduit $ bs .| C.mapM_ send
      where
        send b = write b *> flush
