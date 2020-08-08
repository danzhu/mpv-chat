module Network.Wai.Monad
  ( Wai
  , runWai
  , wai
  ) where

import           Network.Wai                    ( Application
                                                , Request
                                                , Response
                                                , ResponseReceived
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(ReaderT)
                                                , runReaderT
                                                )
import           Control.Monad.Trans.Cont       ( ContT(ContT)
                                                , runContT
                                                )

type Wai = ReaderT Request (ContT ResponseReceived IO)

wai :: Application -> Wai Response
wai app = ReaderT $ ContT . app

runWai :: Wai Response -> Application
runWai app = runContT . runReaderT app
