module Network.Wai.IO
  ( Event(..)
  , requestJson
  , requestSource
  , responseEvents
  , responsePlainStatus
  , responseSource
  ) where

import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson                     ( Value
                                                , json'
                                                )
import qualified Data.ByteString               as B
import           Data.ByteString.Builder        ( Builder
                                                , byteString
                                                , intDec
                                                , lazyByteString
                                                )
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LBC
import           Data.Conduit                   ( ConduitT
                                                , runConduit
                                                , yield
                                                , (.|)
                                                )
import           Data.Conduit.Attoparsec        ( sinkParser )
import qualified Data.Conduit.Combinators      as C
import           Data.Default.Class             ( Default
                                                , def
                                                )
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
  { eventData :: LB.ByteString
  }
  deriving Show

instance Default Event where
  def = Event ""

requestSource :: MonadIO m => Request -> ConduitT i B.ByteString m ()
requestSource req = do
  chunk <- liftIO $ getRequestBodyChunk req
  unless (B.null chunk) $ do
    yield chunk
    requestSource req

requestJson :: MonadIO m => Request -> m Value
requestJson req = liftIO $ runConduit $ requestSource req .| sinkParser json'

responsePlainStatus :: Status -> ResponseHeaders -> Response
responsePlainStatus s hs = responseBuilder s hs' b where
  hs' = (hContentType, "text/plain; charset=utf-8") : hs
  b = intDec (statusCode s) <> " " <> byteString (statusMessage s) <> "\n"

responseSource :: Status -> ResponseHeaders -> ConduitT () Builder IO () -> Response
responseSource s hs bs = responseStream s hs body where
  body write flush = runConduit $ bs .| C.mapM_ send where
    send b = write b *> flush

responseEvents :: ConduitT () Event IO () -> Response
responseEvents evts = responseSource ok200 hs $ evts .| C.map fmt where
  hs = [(hContentType, "text/event-stream")]
  fmt (Event d) = foldMap dat (LBC.split '\n' d) <> "\n" where
    dat c = "data: " <> lazyByteString c <> "\n"
