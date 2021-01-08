module Network.Wai.Middleware.StaticRoute
  ( routeAccept,
    routeGet,
    routeMethod,
    routePost,
  )
where

import Network.HTTP.Media
  ( MediaType,
    mapAccept,
  )
import Network.HTTP.Types.Header
  ( hAccept,
    hAllow,
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
    methodNotAllowed405,
    notAcceptable406,
    ok200,
  )
import Network.Wai
  ( Application,
    requestHeaders,
    requestMethod,
    responseBuilder,
  )

routeMethod :: (Status -> Application) -> [(Method, Application)] -> Application
routeMethod err apps req res
  | Just app <- lookup m apps = app req res
  | m == methodOptions = do
    let hs = [(hAllow, intercalate "," $ fst <$> apps)]
    res $ responseBuilder ok200 hs mempty
  | otherwise = err methodNotAllowed405 req res
  where
    m = requestMethod req

routeAccept :: (Status -> Application) -> [(MediaType, Application)] -> Application
routeAccept err apps = join $ fromMaybe none . mapAccept apps . accept
  where
    none = err notAcceptable406
    accept = fromMaybe "*/*" . lookup hAccept . requestHeaders

routeGet :: (Status -> Application) -> Application -> Application
routeGet err app =
  routeMethod
    err
    [ (methodGet, app),
      -- TODO: manually check that no body is sent
      (methodHead, app)
    ]

routePost :: (Status -> Application) -> Application -> Application
routePost err app = routeMethod err [(methodPost, app)]
