module Network.Request
  ( request,
  )
where

import Data.Aeson (FromJSON)
import Network.HTTP.Simple
  ( getResponseBody,
    httpJSON,
    parseRequestThrow,
    setRequestHeaders,
    setRequestQueryString,
  )
import Network.HTTP.Types
  ( Query,
    RequestHeaders,
  )

request :: (MonadIO m, FromJSON a) => Text -> Query -> RequestHeaders -> m a
request url query headers = do
  req <- liftIO $ parseRequestThrow $ toList url
  res <-
    httpJSON $
      req
        & setRequestHeaders headers
        & setRequestQueryString query
  pure $ getResponseBody res
