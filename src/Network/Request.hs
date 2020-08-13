module Network.Request
  ( request
  ) where

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

request :: (MonadIO m, FromJSON a) => Text -> Query -> RequestHeaders -> m a
request url query headers = do
  req <- liftIO $ parseRequestThrow $ toList url
  res <- httpJSON $ req
    & setRequestHeaders headers
    & setRequestQueryString query
  pure $ getResponseBody res
