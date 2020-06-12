module Network.Request
  ( request
  ) where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson                     ( FromJSON )
import           Data.Function                  ( (&) )
import qualified Data.Text                     as T
import           Network.HTTP.Simple            ( setRequestHeaders
                                                , getResponseBody
                                                , httpJSON
                                                , parseRequestThrow
                                                , setRequestQueryString
                                                )
import           Network.HTTP.Types             ( Query
                                                , RequestHeaders
                                                )

request :: (MonadIO m, FromJSON a) => T.Text -> Query -> RequestHeaders -> m a
request url query headers = do
  req <- liftIO $ parseRequestThrow $ T.unpack url
  res <- httpJSON $ req
    & setRequestHeaders headers
    & setRequestQueryString query
  pure $ getResponseBody res
