module Network.Request
  ( request,
    statusErr,
  )
where

import Data.Aeson (FromJSON)
import Network.HTTP.Client.Conduit
  ( HttpExceptionContent (StatusCodeException),
    Response (responseStatus),
  )
import Network.HTTP.Simple
  ( HttpException (HttpExceptionRequest),
    getResponseBody,
    httpJSON,
    parseRequestThrow,
    setRequestHeaders,
    setRequestQueryString,
  )
import Network.HTTP.Types
  ( Query,
    RequestHeaders,
    Status,
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

statusErr :: (Status -> Bool) -> HttpException -> Maybe Status
statusErr p (HttpExceptionRequest _ (StatusCodeException (responseStatus -> s) _))
  | p s = Just s
statusErr _ _ = Nothing
