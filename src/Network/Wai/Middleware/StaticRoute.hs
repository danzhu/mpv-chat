module Network.Wai.Middleware.StaticRoute
  ( routeAccept
  , routeMethod
  ) where

import qualified Data.ByteString.Char8         as BC
import           Data.Maybe                     ( fromMaybe )
import           Network.HTTP.Types.Header      ( hAccept
                                                , hAllow
                                                )
import           Network.HTTP.Types.Method      ( Method
                                                , methodOptions
                                                )
import           Network.HTTP.Types.Status      ( Status
                                                , methodNotAllowed405
                                                , notAcceptable406
                                                , ok200
                                                )
import           Network.HTTP.Media             ( MediaType
                                                , mapAccept
                                                )
import           Network.Wai                    ( Application
                                                , requestMethod
                                                , requestHeaders
                                                , responseBuilder
                                                )
import           Control.Monad                  ( join )

routeMethod :: (Status -> Application) -> [(Method, Application)] -> Application
routeMethod err apps req res
  | Just app <- lookup m apps = app req res
  | m == methodOptions = do
      let hs = [(hAllow, BC.intercalate "," $ map fst apps)]
      res $ responseBuilder ok200 hs mempty
  | otherwise = err methodNotAllowed405 req res
  where
    m = requestMethod req

routeAccept :: (Status -> Application) -> [(MediaType, Application)] -> Application
routeAccept err apps = join $ fromMaybe def . mapAccept apps . accept where
  def = err notAcceptable406
  accept = fromMaybe "*/*" . lookup hAccept . requestHeaders
