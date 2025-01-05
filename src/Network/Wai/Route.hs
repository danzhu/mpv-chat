{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Route
  ( -- * Route
    Route,
    route,
    route',
    routeAny,
    routeAny',
    runRoute,

    -- * Response Types
    ToResponse (..),
    WaiBody (..),
    WithStatus (..),

    -- * Common Routes
    redirect,
    Event (..),
    eventsRoute,
    fileRoute,
    staticRoute,
  )
where

import Data.Aeson (ToJSON (toEncoding), Value, fromEncoding)
import Data.ByteString.Builder
  ( byteString,
    intDec,
    lazyByteString,
  )
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid (mconcat))
import Lucid (Html, execHtmlT)
import Network.HTTP.Media
  ( MediaType,
    RenderHeader (renderHeader),
    mapAccept,
  )
import Network.HTTP.Media.Accept (parseAccept)
import Network.HTTP.Types.Header
  ( hAccept,
    hAllow,
    hContentLength,
    hContentType,
  )
import Network.HTTP.Types.Method
  ( Method,
    StdMethod (GET, HEAD, OPTIONS),
    methodGet,
    renderStdMethod,
  )
import Network.HTTP.Types.Status
  ( Status,
    forbidden403,
    methodNotAllowed405,
    noContent204,
    notAcceptable406,
    notFound404,
    ok200,
  )
import Network.Mime
  ( MimeType,
    defaultMimeMap,
    fileNameExtensions,
  )
import Network.Wai
  ( FilePart,
    Request,
    Response,
    StreamingBody,
    mapResponseHeaders,
    mapResponseStatus,
    requestHeaders,
    requestMethod,
    responseBuilder,
    responseFile,
    responseStream,
  )
import Network.Wai.Handler.Warp (getFileInfo)
import Network.Wai.Monad
  ( Wai,
    WaiApp,
    responseRedirect,
    responseSource,
    throwWai,
  )
import System.FilePath
  ( joinPath,
    takeFileName,
  )
import System.IO.Error (isDoesNotExistError)

data RouteAccept = RouteAccept
  { medias :: [(MediaType, WaiApp)],
    fallback :: Maybe WaiApp
  }

makeFieldLabelsNoPrefix ''RouteAccept

data RouteMethod = RouteMethod
  { methods :: Map Method RouteAccept,
    fallback :: Maybe RouteAccept
  }

makeFieldLabelsNoPrefix ''RouteMethod

instance Semigroup RouteAccept where
  RouteAccept m fb <> RouteAccept m' fb' =
    RouteAccept (m `union` m') (fb <|> fb')

instance Monoid RouteAccept where
  mempty = RouteAccept [] Nothing

instance Semigroup RouteMethod where
  RouteMethod m fb <> RouteMethod m' fb' =
    RouteMethod (unionWith (<>) m m') (fb <|> fb')

instance Monoid RouteMethod where
  mempty = RouteMethod mempty Nothing

newtype Route a = Route (RWST Request RouteMethod () IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Request)

route :: forall a. (ToResponse a) => StdMethod -> Wai a -> Route ()
route method = route' method (mediaType @a Proxy)

route' :: (ToResponse a) => StdMethod -> Maybe MediaType -> Wai a -> Route ()
route' method media app =
  Route $ tell $ routeMedia method media $ toResponse <$> app

routeAny :: forall a. (ToResponse a) => Wai a -> Route ()
routeAny = routeAny' (mediaType @a Proxy)

routeAny' :: forall a. (ToResponse a) => Maybe MediaType -> Wai a -> Route ()
routeAny' media app =
  Route $ tell $ mempty & #fallback ?!~ routeAccept media (toResponse <$> app)

routeMedia :: StdMethod -> Maybe MediaType -> WaiApp -> RouteMethod
routeMedia method media app = routeMethod method $ routeAccept media app

routeAccept :: Maybe MediaType -> WaiApp -> RouteAccept
routeAccept media app =
  mempty & case media of
    Just m -> #medias !~ singletonMap m app
    Nothing -> #fallback ?!~ app

routeMethod :: StdMethod -> RouteAccept -> RouteMethod
routeMethod method ra = mempty & #methods % at (renderStdMethod method) ?!~ ra

responseAllow :: Status -> RouteMethod -> Response
responseAllow s RouteMethod {methods} = responseBuilder s hs mempty
  where
    hs =
      [ (hAllow, intercalate "," $ keys methods),
        (hContentLength, "0")
      ]

insertAutoRoutes :: RouteMethod -> RouteMethod
insertAutoRoutes rm@RouteMethod {methods} = rm'
  where
    rm' = mconcat [rm, rOptions, rHead]
    rOptions = routeMedia OPTIONS Nothing do
      pure $ responseAllow noContent204 rm'
    rHead = case lookup methodGet methods of
      Nothing -> mempty
      Just rMedia -> routeMethod HEAD rMedia

runRoute :: Route () -> WaiApp
runRoute (Route r) = do
  req <- ask
  ((), (), rm) <- liftIO $ runRWST r req ()
  runRouteMethod rm

runRouteMethod :: RouteMethod -> WaiApp
runRouteMethod RouteMethod {methods = Empty, fallback = Nothing} =
  throwWai notFound404 ""
runRouteMethod rm = do
  let rm'@RouteMethod {methods, fallback} = insertAutoRoutes rm
      notAllowed = pure $ responseAllow methodNotAllowed405 rm'
  m <- asks requestMethod
  maybe notAllowed runRouteAccept $ lookup m methods <|> fallback

runRouteAccept :: RouteAccept -> WaiApp
runRouteAccept RouteAccept {medias, fallback} = do
  accept <- asks $ fromMaybe "*/*" . lookup hAccept . requestHeaders
  let (mime, app) =
        fromMaybe (Nothing, notAcceptable) $
          mapAccept (map addMime medias) accept
            <|> ((Nothing,) <$> fallback)
            <|> (first Just <$> medias ^? _head)
      hs = mapMaybe sequence [(hContentType, renderHeader <$> mime)]
  mapResponseHeaders (`union` hs) <$> app
  where
    notAcceptable = throwWai notAcceptable406 ""
    addMime (m, a) = (m, (Just m, a))

-- data WaiResponse = WaiResponse
--   { status :: Status,
--     headers :: ResponseHeaders,
--     body :: WaiBody
--   }

data WaiBody
  = BuilderBody Builder
  | FileBody FilePath (Maybe FilePart)
  | StreamBody StreamingBody

data WithStatus a = WithStatus Status a

class ToResponse a where
  mediaType :: proxy a -> Maybe MediaType
  mediaType _ = Nothing
  toBody :: a -> (Maybe Int, WaiBody)
  toResponse :: a -> Response
  toResponse a = case body of
    BuilderBody b -> responseBuilder ok200 hs b
    FileBody f p -> responseFile ok200 hs f p
    StreamBody b -> responseStream ok200 hs b
    where
      (len, body) = toBody a
      hs =
        mapMaybe
          sequence
          [ (hContentLength, encodeUtf8 . tshow <$> len)
          ]

instance ToResponse Response where
  toBody = error "toBody: not supported for Response"
  toResponse = identity

instance (ToResponse a) => ToResponse (WithStatus a) where
  mediaType _ = mediaType @a Proxy
  toBody (WithStatus _ a) = toBody a
  toResponse (WithStatus s a) = mapResponseStatus (const s) $ toResponse a

instance ToResponse () where
  toBody () = (Just 0, BuilderBody mempty)

instance ToResponse Builder where
  toBody = (Nothing,) . BuilderBody

instance ToResponse ByteString where
  mediaType _ = Just "application/octet-stream"
  toBody bs = (Just $ length bs, BuilderBody $ byteString bs)

instance ToResponse Text where
  mediaType _ = Just "text/plain; charset=utf-8"
  toBody t = (Just $ length bs, BuilderBody $ byteString bs)
    where
      bs = encodeUtf8 t

instance ToResponse Value where
  mediaType _ = Just "application/json"
  toBody = (Nothing,) . BuilderBody . fromEncoding . toEncoding

instance ToResponse (Html a) where
  mediaType _ = Just "text/html"
  toBody = (Nothing,) . BuilderBody . runIdentity . execHtmlT

redirect :: ByteString -> Route ()
redirect = routeAny . pure . responseRedirect

data Event = Event
  { event :: Maybe ByteString,
    data_ :: LByteString,
    id :: Maybe ByteString,
    retry :: Maybe Int
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''Event

instance Default Event where
  def =
    Event
      { event = Nothing,
        data_ = "",
        id = Nothing,
        retry = Nothing
      }

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

eventsRoute :: ConduitT () Event IO () -> Route ()
eventsRoute evts = route' GET (Just "text/event-stream") do
  pure $
    responseSource ok200 [] $
      evts .| C.map eventToBuilder

mimeByExt :: Text -> Maybe MimeType
mimeByExt = asum . map (`lookup` defaultMimeMap) . fileNameExtensions

fileRoute :: FilePath -> Route ()
fileRoute fp = route' GET media $ pure $ responseFile ok200 [] fp Nothing
  where
    media = parseAccept =<< mimeByExt (fromList $ takeFileName fp)

staticRoute :: FilePath -> [Text] -> Route ()
staticRoute dir path
  | any (isPrefixOf ".") path = throwWai forbidden403 "hidden path"
  | maybe True null $ lastOf each path = redirect "index.html"
  | otherwise = do
      let fp = joinPath $ dir : map toList path
      req <- ask
      info <- liftIO $ tryJust (guard . isDoesNotExistError) $ getFileInfo req fp
      for_ info $ \_ -> fileRoute fp
