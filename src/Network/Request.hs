module Network.Request
  ( request
  ) where

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
import           MpvChat.Prelude

request :: (MonadIO m, FromJSON a) => T.Text -> Query -> RequestHeaders -> m a
request url query headers = do
  req <- liftIO $ parseRequestThrow $ T.unpack url
  res <- httpJSON $ req
    & setRequestHeaders headers
    & setRequestQueryString query
  pure $ getResponseBody res
