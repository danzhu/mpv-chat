module Network.Wai.Application.Static
  ( appStatic
  ) where

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import           MpvChat.Prelude
import           Network.HTTP.Media.Accept      ( parseAccept )
import           Network.HTTP.Types.Header      ( hContentType
                                                , hLocation
                                                )
import           Network.HTTP.Types.Method      ( methodGet
                                                , methodHead
                                                )
import           Network.HTTP.Types.Status      ( Status
                                                , forbidden403
                                                , notFound404
                                                , ok200
                                                , temporaryRedirect307
                                                )
import           Network.Mime                   ( FileName
                                                , MimeType
                                                , defaultMimeMap
                                                , fileNameExtensions
                                                )
import           Network.Wai                    ( Application
                                                , pathInfo
                                                , responseFile
                                                )
import           Network.Wai.Handler.Warp       ( getFileInfo )
import           Network.Wai.IO                 ( responsePlainStatus )
import           Network.Wai.Middleware.StaticRoute
                                                ( routeAccept
                                                , routeMethod
                                                )
import           System.FilePath                ( joinPath )

mimeByExt :: FileName -> Maybe MimeType
mimeByExt = asum . fmap (defaultMimeMap M.!?) . fileNameExtensions

appStatic :: (Status -> Application) -> FilePath -> Application
appStatic err dir = app where
  app = routeMethod err
    [ (methodGet, get)
    , (methodHead, get)
    ]
  get req res
    | any (T.isPrefixOf ".") path = err forbidden403 req res
    | T.null fn = do
        let hs = [(hLocation, "index.html")]
        -- TODO: extract responsePlainStatus
        res $ responsePlainStatus temporaryRedirect307 hs
    | Just mime@(parseAccept -> Just media) <- mimeByExt fn =
        routeAccept err [(media, file [(hContentType, mime)] fp)] req res
    | otherwise = file [] fp req res
    where
      path = pathInfo req
      -- due to Monoid instance, empty path returns empty string
      fn = path ^. _last
      fp = joinPath $ dir : fmap T.unpack path
  file hs fp req res = try (getFileInfo req fp) >>= \case
    Left (_ :: IOException) -> err notFound404 req res
    Right _ -> res $ responseFile ok200 hs fp Nothing
