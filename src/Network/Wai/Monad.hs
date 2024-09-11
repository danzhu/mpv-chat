module Network.Wai.Monad
  ( Wai,
    WaiApp,
    ErrorResponse (..),
    Event (..),
    wai,
    runWai,
    throwWai,
    routeMethod,
    routeAccept,
    routeGet,
    routePost,
    requestSource,
    requestBS,
    requestJson,
    responsePlainStatus,
    responseRedirect,
    responseSource,
    responseEvents,
    appFile,
    appStatic,
  )
where

import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson
  ( Value,
    json',
  )
import Data.ByteString.Builder
  ( byteString,
    intDec,
    lazyByteString,
  )
import Data.Conduit (ConduitT, runConduit, yield, (.|))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.Combinators as C
import Network.HTTP.Media
  ( MediaType,
    mapAccept,
  )
import Network.HTTP.Media.Accept (parseAccept)
import Network.HTTP.Types.Header
  ( ResponseHeaders,
    hAccept,
    hAllow,
    hContentType,
    hLocation,
  )
import Network.HTTP.Types.Method
  ( Method,
    methodGet,
    methodHead,
    methodOptions,
    methodPost,
  )
import Network.HTTP.Types.Status
  ( Status,
    forbidden403,
    methodNotAllowed405,
    noContent204,
    notAcceptable406,
    notFound404,
    ok200,
    statusCode,
    statusMessage,
    temporaryRedirect307,
  )
import Network.Mime
  ( MimeType,
    defaultMimeMap,
    fileNameExtensions,
  )
import Network.Wai
  ( Application,
    Request,
    Response,
    ResponseReceived,
    getRequestBodyChunk,
    pathInfo,
    requestHeaders,
    requestMethod,
    responseBuilder,
    responseFile,
    responseStream,
  )
import Network.Wai.Handler.Warp (getFileInfo)
import System.FilePath
  ( joinPath,
    takeFileName,
  )
import System.IO.Error (isDoesNotExistError)

data Event = Event
  { event :: Maybe ByteString,
    data_ :: LByteString,
    id :: Maybe ByteString,
    retry :: Maybe Int
  }
  deriving stock (Show)

instance Default Event where
  def =
    Event
      { event = Nothing,
        data_ = "",
        id = Nothing,
        retry = Nothing
      }

type Wai = ReaderT Request (ContT ResponseReceived IO)

type WaiApp = Wai Response

wai :: Application -> WaiApp
wai app = ReaderT $ ContT . app

runWai :: WaiApp -> Application
runWai app = runContT . runReaderT app

data ErrorResponse = ErrorResponse {status :: Status, message :: Text}
  deriving stock (Show)
  deriving anyclass (Exception)

throwWai :: (MonadThrow m) => Status -> Text -> m a
throwWai status msg = throwM $ ErrorResponse status msg

routeMethod :: [(Method, WaiApp)] -> WaiApp
routeMethod apps = do
  m <- asks requestMethod
  let allow = intercalate "," $ map fst apps
      opts = pure $ responseBuilder noContent204 [(hAllow, allow)] mempty
  fromMaybe (throwWai methodNotAllowed405 "") $ lookup m $ apps <> [(methodOptions, opts)]

routeAccept :: [(MediaType, WaiApp)] -> WaiApp
routeAccept apps = do
  accept <- asks $ fromMaybe "*/*" . lookup hAccept . requestHeaders
  fromMaybe (throwWai notAcceptable406 "") $ mapAccept apps accept

routeGet :: WaiApp -> WaiApp
routeGet app =
  routeMethod
    [ (methodGet, app),
      -- TODO: manually check that no body is sent
      (methodHead, app)
    ]

routePost :: WaiApp -> WaiApp
routePost app = routeMethod [(methodPost, app)]

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

responsePlainStatus :: Status -> ResponseHeaders -> Response
responsePlainStatus s hs = responseBuilder s hs' b
  where
    hs' = (hContentType, "text/plain; charset=utf-8") : hs
    -- TODO: also show headers
    b = intDec (statusCode s) <> " " <> byteString (statusMessage s) <> "\n"

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

eventToBuilder :: Event -> Builder
eventToBuilder Event {event, data_, id, retry} =
  fold
    [ flip foldMap event \e ->
        "event: " <> byteString e <> "\n",
      flip foldMap (splitSeq "\n" data_) \c ->
        "data: " <> lazyByteString c <> "\n",
      flip foldMap id \i ->
        "id: " <> byteString i <> "\n",
      flip foldMap retry \r ->
        "retry: " <> intDec r <> "\n",
      "\n"
    ]

responseEvents :: ConduitT () Event IO () -> Response
responseEvents evts =
  responseSource ok200 [(hContentType, "text/event-stream")] $
    evts .| C.map eventToBuilder

mimeByExt :: Text -> Maybe MimeType
mimeByExt = asum . map (`lookup` defaultMimeMap) . fileNameExtensions

appFile :: FilePath -> WaiApp
appFile fp = case mimeByExt $ fromList $ takeFileName fp of
  Just mime@(parseAccept -> Just media) ->
    routeAccept [(media, file [(hContentType, mime)])]
  _ -> file []
  where
    file :: ResponseHeaders -> WaiApp
    file hs = do
      req <- ask
      liftIO (tryJust (guard . isDoesNotExistError) $ getFileInfo req fp) >>= \case
        Left () -> throwWai notFound404 ""
        Right _ -> pure $ responseFile ok200 hs fp Nothing

appStatic :: FilePath -> WaiApp
appStatic dir = do
  path <- asks pathInfo
  if
    | any (isPrefixOf ".") path -> throwWai forbidden403 "hidden path"
    | maybe True null $ lastOf each path -> pure $ responseRedirect "index.html"
    | otherwise -> routeGet $ appFile $ joinPath $ dir : map toList path
