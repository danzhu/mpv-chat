module Network.Wai.Monad
  ( App
  , application
  , runApp
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

type App = ReaderT Request (ContT ResponseReceived IO)

application :: Application -> App Response
application app = ReaderT $ ContT . app

runApp :: App Response -> Application
runApp app = runContT . runReaderT app
