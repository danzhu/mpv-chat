{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Response
  ( responseEvents
  , responseStatus
  ) where

import           Data.ByteString.Builder        ( byteString
                                                , intDec
                                                , lazyByteString
                                                )
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LBC
import           Data.Conduit                   ( ConduitT
                                                , runConduit
                                                , (.|)
                                                )
import qualified Data.Conduit.Combinators      as C
import           Data.Foldable                  ( traverse_ )
import           Network.HTTP.Types.Header      ( ResponseHeaders
                                                , hContentType
                                                )
import           Network.HTTP.Types.Status      ( Status
                                                , ok200
                                                , statusCode
                                                , statusMessage
                                                )
import           Network.Wai                    ( Response
                                                , StreamingBody
                                                , responseBuilder
                                                , responseStream
                                                )

responseStatus :: Status -> ResponseHeaders -> Response
responseStatus s hs = responseBuilder s hs' b where
  hs' = (hContentType, "text/plain; charset=utf-8") : hs
  b = intDec (statusCode s) <> " " <> byteString (statusMessage s) <> "\n"

responseEvents :: ConduitT () LB.ByteString IO () -> Response
responseEvents evts = responseStream ok200 hs body where
  hs = [(hContentType, "text/event-stream")]
  body :: StreamingBody
  body write flush = runConduit $ evts .| C.mapM_ send where
    dat c = write $ "data: " <> lazyByteString c <> "\n"
    send e = do
      traverse_ dat $ LBC.split '\n' e
      write "\n"
      flush
