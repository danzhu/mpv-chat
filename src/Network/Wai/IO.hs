module Network.Wai.IO
  ( Event(..)
  , requestJson
  , requestSource
  , responseEvents
  , responsePlainStatus
  , responseSource
  ) where

import           Data.Aeson                     ( Value
                                                , json'
                                                )
import           Data.ByteString.Builder        ( Builder
                                                , byteString
                                                , intDec
                                                , lazyByteString
                                                )
import           Data.Conduit.Attoparsec        ( sinkParser )
import qualified Data.Conduit.Combinators      as C
import           MpvChat.Prelude
import           Network.HTTP.Types.Header      ( ResponseHeaders
                                                , hContentType
                                                )
import           Network.HTTP.Types.Status      ( Status
                                                , ok200
                                                , statusCode
                                                , statusMessage
                                                )
import           Network.Wai                    ( Request
                                                , Response
                                                , getRequestBodyChunk
                                                , responseBuilder
                                                , responseStream
                                                )

newtype Event = Event
  { eventData :: LByteString
  }
  deriving stock Show

instance Default Event where
  def = Event ""

requestSource :: MonadIO m => Request -> ConduitT i ByteString m ()
requestSource req = do
  chunk <- liftIO $ getRequestBodyChunk req
  unless (null chunk) $ do
    yield chunk
    requestSource req

requestJson :: MonadIO m => Request -> m Value
requestJson req = liftIO $ runConduit $ requestSource req .| sinkParser json'

responsePlainStatus :: Status -> ResponseHeaders -> Response
responsePlainStatus s hs = responseBuilder s hs' b where
  hs' = (hContentType, "text/plain; charset=utf-8") : hs
  -- TODO: also show headers
  b = intDec (statusCode s) <> " " <> byteString (statusMessage s) <> "\n"

responseSource :: Status -> ResponseHeaders -> ConduitT () Builder IO () -> Response
responseSource s hs bs = responseStream s hs body where
  body write flush = runConduit $ bs .| C.mapM_ send where
    send b = write b *> flush

responseEvents :: ConduitT () Event IO () -> Response
responseEvents evts = responseSource ok200 hs $ evts .| C.map fmt where
  hs = [(hContentType, "text/event-stream")]
  fmt (Event d) = foldMap dat (splitSeq "\n" d) <> "\n" where
    dat c = "data: " <> lazyByteString c <> "\n"
