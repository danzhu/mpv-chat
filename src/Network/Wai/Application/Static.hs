module Network.Wai.Application.Static
  ( appStatic
  ) where

import           Control.Exception              ( IOException
                                                , try
                                                )
import           Data.Foldable                  ( asum )
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import           Lens.Micro                     ( _last
                                                , each
                                                , to
                                                , (^..)
                                                )
import           Network.HTTP.Types.Header      ( hContentType )
import           Network.HTTP.Types.Method      ( methodGet
                                                , methodHead
                                                )
import           Network.HTTP.Types.Status      ( Status
                                                , forbidden403
                                                , methodNotAllowed405
                                                , notFound404
                                                , ok200
                                                )
import           Network.Mime                   ( FileName
                                                , MimeType
                                                , defaultMimeMap
                                                , fileNameExtensions
                                                )
import           Network.Wai                    ( Application
                                                , pathInfo
                                                , requestMethod
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
appStatic err dir req res
  | requestMethod req `notElem` [methodGet, methodHead] =
      err methodNotAllowed405 req res
  | any hiddenFile path = err forbidden403 req res
  | otherwise = try (getFileInfo req fp) >>= \case
      Left (_ :: IOException) -> err notFound404 req res
      Right _ -> res $ responseFile ok200 hs fp Nothing
  where
    path = routeStatic $ pathInfo req
    fp = joinPath $ map T.unpack $ dir : path
    hs = path ^.. _last . to mimeByExt . each . to (hContentType,)
