module Network.Wai.Monad
  ( Wai,
    WaiApp,
    Event (..),
    runWai,
    wai,
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

import Data.Aeson
  ( Value,
    json',
  )
import Data.ByteString.Builder
  ( Builder,
    byteString,
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

newtype Event = Event
  { eventData :: LByteString
  }
  deriving stock (Show)

instance Default Event where
  def = Event ""

type Wai = ReaderT Request (ContT ResponseReceived IO)

type WaiApp = Wai Response

wai :: Application -> WaiApp
wai app = ReaderT $ ContT . app

runWai :: WaiApp -> Application
runWai app = runContT . runReaderT app

routeMethod ::
  (Status -> WaiApp) ->
  [(Method, WaiApp)] ->
  WaiApp
routeMethod err apps = do
  m <- asks requestMethod
  let allow = intercalate "," $ map fst apps
      opts = pure $ responseBuilder noContent204 [(hAllow, allow)] mempty
  fromMaybe (err methodNotAllowed405) $ lookup m $ apps <> [(methodOptions, opts)]

routeAccept ::
  (Status -> WaiApp) ->
  [(MediaType, WaiApp)] ->
  WaiApp
routeAccept err apps = do
  accept <- asks $ fromMaybe "*/*" . lookup hAccept . requestHeaders
  fromMaybe (err notAcceptable406) $ mapAccept apps accept

routeGet :: (Status -> WaiApp) -> WaiApp -> WaiApp
routeGet err app =
  routeMethod
    err
    [ (methodGet, app),
      -- TODO: manually check that no body is sent
      (methodHead, app)
    ]

routePost :: (Status -> WaiApp) -> WaiApp -> WaiApp
routePost err app = routeMethod err [(methodPost, app)]

requestSource :: MonadIO m => Request -> ConduitT i ByteString m ()
requestSource req = do
  chunk <- liftIO $ getRequestBodyChunk req
  unless (null chunk) $ do
    yield chunk
    requestSource req

requestBS :: MonadIO m => Request -> m ByteString
requestBS req = liftIO $ runConduit $ requestSource req .| C.fold

requestJson :: MonadIO m => Request -> m Value
requestJson req = liftIO $ runConduit $ requestSource req .| sinkParser json'

responsePlainStatus :: Status -> ResponseHeaders -> Response
responsePlainStatus s hs = responseBuilder s hs' b
  where
    hs' = (hContentType, "text/plain; charset=utf-8") : hs
    -- TODO: also show headers
    b = intDec (statusCode s) <> " " <> byteString (statusMessage s) <> "\n"

responseRedirect :: ByteString -> Response
-- TODO: show location in body (maybe link), and optionally js redirect
responseRedirect l = responsePlainStatus temporaryRedirect307 [(hLocation, l)]

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

responseEvents :: ConduitT () Event IO () -> Response
responseEvents evts = responseSource ok200 hs $ evts .| C.map fmt
  where
    hs = [(hContentType, "text/event-stream")]
    fmt (Event d) = foldMap dat (splitSeq "\n" d) <> "\n"
      where
        dat c = "data: " <> lazyByteString c <> "\n"

mimeByExt :: Text -> Maybe MimeType
mimeByExt = asum . map (`lookup` defaultMimeMap) . fileNameExtensions

appFile :: (Status -> WaiApp) -> FilePath -> WaiApp
appFile err fp = case mimeByExt $ fromList $ takeFileName fp of
  Just mime@(parseAccept -> Just media) ->
    routeAccept err [(media, file [(hContentType, mime)])]
  _ -> file []
  where
    file :: ResponseHeaders -> WaiApp
    file hs = do
      req <- ask
      liftIO (try $ getFileInfo req fp) >>= \case
        Left (_ :: IOException) -> err notFound404
        Right _ -> pure $ responseFile ok200 hs fp Nothing

appStatic :: (Status -> WaiApp) -> FilePath -> WaiApp
appStatic err dir = do
  path <- asks pathInfo
  if
      | any (isPrefixOf ".") path -> err forbidden403
      | maybe True null $ lastOf each path -> pure $ responseRedirect "index.html"
      | otherwise -> routeGet err $ appFile err $ joinPath $ dir : map toList path
