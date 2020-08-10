module Network.Wai.Monad
  ( Wai
  , runWai
  , wai
  ) where

import           MpvChat.Prelude
import           Network.Wai                    ( Application
                                                , Request
                                                , Response
                                                , ResponseReceived
                                                )

type Wai = ReaderT Request (ContT ResponseReceived IO)

wai :: Application -> Wai Response
wai app = ReaderT $ ContT . app

runWai :: Wai Response -> Application
runWai app = runContT . runReaderT app
