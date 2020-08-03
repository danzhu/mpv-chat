module Network.Wai.Application.Static
  ( appStatic
  ) where

import           Network.Wai.Middleware.StaticRoute
                                                ( routeAccept
                                                , routeMethod
                                                )

import           Control.Exception              ( IOException
                                                , try
                                                )
import           Data.Foldable                  ( asum )
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import           Lens.Micro                     ( _last
                                                , (^.)
                                                )
import           Network.HTTP.Types.Header      ( hContentType )
import           Network.HTTP.Types.Method      ( methodGet
                                                , methodHead
                                                )
import           Network.HTTP.Types.Status      ( Status
                                                , forbidden403
                                                , notFound404
                                                , ok200
                                                )
import           Network.HTTP.Media.Accept      ( parseAccept )
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
import           System.FilePath                ( joinPath )

mimeByExt :: FileName -> Maybe MimeType
mimeByExt = asum . map (defaultMimeMap M.!?) . fileNameExtensions

hiddenFile :: T.Text -> Bool
hiddenFile s
  | Just ('.', _) <- T.uncons s = True
  | otherwise = False

routeStatic :: [T.Text] -> [T.Text]
routeStatic [] = ["index.html"]
routeStatic p  = p

appStatic :: (Status -> Application) -> T.Text -> Application
appStatic err dir = app where
  app = routeMethod err
    [ (methodGet, get)
    , (methodHead, get)
    ]
  get req
    | any hiddenFile path = err forbidden403 req
    | Just mime@(parseAccept -> Just media) <- mimeByExt $ path ^. _last =
        routeAccept err [(media, file [(hContentType, mime)] fp)] req
    | otherwise = file [] fp req
    where
      path = routeStatic $ pathInfo req
      fp = joinPath $ map T.unpack $ dir : path
  file hs fp req res = try (getFileInfo req fp) >>= \case
    Left (_ :: IOException) -> err notFound404 req res
    Right _ -> res $ responseFile ok200 hs fp Nothing
