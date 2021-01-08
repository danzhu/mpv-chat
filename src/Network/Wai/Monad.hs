module Network.Wai.Monad
  ( Wai,
    WaiApp,
    runWai,
    wai,
  )
where

import MpvChat.Prelude
import Network.Wai
  ( Application,
    Request,
    Response,
    ResponseReceived,
  )

type Wai = ReaderT Request (ContT ResponseReceived IO)

type WaiApp = Wai Response

wai :: Application -> WaiApp
wai app = ReaderT $ ContT . app

runWai :: WaiApp -> Application
runWai app = runContT . runReaderT app
