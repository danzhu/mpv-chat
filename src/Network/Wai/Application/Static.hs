module Network.Wai.Application.Static
  ( appFile,
    appStatic,
  )
where

import Network.HTTP.Media.Accept (parseAccept)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status
  ( Status,
    forbidden403,
    notFound404,
    ok200,
  )
import Network.Mime
  ( FileName,
    MimeType,
    defaultMimeMap,
    fileNameExtensions,
  )
import Network.Wai
  ( Application,
    pathInfo,
    responseFile,
  )
import Network.Wai.Handler.Warp (getFileInfo)
import Network.Wai.IO (responseRedirect)
import Network.Wai.Middleware.StaticRoute
  ( routeAccept,
    routeGet,
  )
import System.FilePath
  ( joinPath,
    takeFileName,
  )

mimeByExt :: FileName -> Maybe MimeType
mimeByExt = asum . map (`lookup` defaultMimeMap) . fileNameExtensions

appFile :: (Status -> Application) -> FilePath -> Application
appFile err fp = case mimeByExt $ fromList $ takeFileName fp of
  Just mime@(parseAccept -> Just media) ->
    routeAccept err [(media, file [(hContentType, mime)])]
  _ -> file []
  where
    file hs req res =
      try (getFileInfo req fp) >>= \case
        Left (_ :: IOException) -> err notFound404 req res
        Right _ -> res $ responseFile ok200 hs fp Nothing

appStatic :: (Status -> Application) -> FilePath -> Application
appStatic err dir req res
  | any (isPrefixOf ".") path = err forbidden403 req res
  | null fn = res $ responseRedirect "index.html"
  | otherwise = routeGet err (appFile err fp) req res
  where
    path = pathInfo req
    -- due to Monoid instance, empty path returns empty string
    fn = path ^. _last
    fp = joinPath $ dir : map toList path
