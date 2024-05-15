{-# LANGUAGE NumericUnderscores #-}

module MpvChat
  ( Config (..),
    runMpvChat,
  )
where

import Control.Concurrent.Task
  ( startTask,
    withEmptyTask,
    withTask_,
  )
import Control.Monad.ContT (contT_)
import Data.Aeson (encode)
import Data.ByteString.Builder (byteString)
import Data.Conduit (ConduitT, yield, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Text.IO (putStrLn)
import Database.SQLite.Simple (withConnection)
import MpvChat.Chat (renderChat)
import MpvChat.Data (View)
import MpvChat.Database (loadEmote, loadVideo)
import MpvChat.Videos (renderVideos)
import Network.HTTP.Types.Status
  ( Status,
    notFound404,
    ok200,
  )
import Network.Mpv
  ( MpvError (MpvIpcError),
    getProperty,
    loadfile,
    newMpvClient,
    observeEvent,
    observeProperty,
    runMpv,
  )
import Network.Wai
  ( pathInfo,
    responseBuilder,
  )
import Network.Wai.Handler.Warp
  ( Port,
    defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setHost,
    setPort,
  )
import Network.Wai.Monad
  ( Wai,
    WaiApp,
    appFile,
    appStatic,
    data_,
    requestBS,
    responseEvents,
    responsePlainStatus,
    responseRedirect,
    routeAccept,
    routeGet,
    routePost,
    runWai,
  )
import System.FilePath.Posix (takeDirectory)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Environment (getExecutablePath)

data Config = Config
  { ipcPath :: FilePath,
    port :: Port
  }
  deriving stock (Show)

err :: Status -> WaiApp
err stat = pure $ responsePlainStatus stat []

page :: ConduitT () View IO () -> WaiApp
page vs =
  routeGet err $
    routeAccept
      err
      [ ("text/html", appFile err "res/page.html"),
        ("text/event-stream", pure $ responseEvents $ vs .| C.map enc)
      ]
  where
    enc v = def {data_ = encode v}

post :: Wai () -> WaiApp
post app = routePost err $ do
  app
  pure $ responsePlainStatus ok200 []

runMpvChat :: Config -> IO ()
runMpvChat Config {ipcPath, port} = evalContT $ do
  conn <- ContT $ withConnection "twitch.db"

  chatState <- newTVarIO def
  active <- newTVarIO False
  seek <- newTVarIO False

  exePath <- getExecutablePath
  let installPath = takeDirectory $ takeDirectory exePath

  mpv <- newMpvClient
  taskLoad <- ContT withEmptyTask

  let entry = putStrLn $ "server started on port " <> tshow port
      settings =
        defaultSettings
          & setHost "*6"
          & setPort port
          & setBeforeMainLoop entry
      messages uid = do
        ver <- newTVarIO (-1)
        forever $ do
          st <- atomically $ do
            st <- readTVar chatState
            let new = st ^. #version
            guard . (< new) =<< readTVar ver
            writeTVar ver new
            pure st
          chat <- liftIO $ renderChat conn st uid
          yield chat
          -- update at most once per 100ms
          threadDelay 100_000
      app =
        asks pathInfo >>= \case
          -- chat messages
          [] -> page $ messages Nothing
          ["user", treadMaybe -> Just uid] -> page $ messages (Just uid)
          ["emote", id] ->
            routeGet err do
              liftIO (loadEmote conn id False) >>= \case
                Just bs -> pure $ responseBuilder ok200 [] $ byteString bs
                Nothing -> err notFound404
          ["emote-third-party", name] ->
            routeGet err do
              liftIO (loadEmote conn name True) >>= \case
                Just bs -> pure $ responseBuilder ok200 [] $ byteString bs
                Nothing -> err notFound404
          -- videos
          ["videos"] -> page $ do
            videos <- liftIO $ renderVideos conn
            yield videos
            threadDelay maxBound
          -- actions
          ["loadfile"] -> post $ do
            url <- join $ asks requestBS
            void $ liftIO $ loadfile mpv $ decodeUtf8 url
          -- docs
          ["doc"] -> pure $ responseRedirect "/doc/all/index.html"
          "doc" : _ -> appStatic err installPath
          -- static
          _ -> appStatic err "public"
      update f = modifyTVar' chatState $ (#version %!~ succ) . f
      load vid = startTask taskLoad $ do
        v <- loadVideo conn vid
        atomically $ do
          -- TODO: fix glitch where playback-time from last video is used before
          -- the new video is loaded
          update $ #video ?!~ v
          writeTVar seek True
      unload =
        startTask taskLoad $
          atomically $
            update $
              #video !~ Nothing
      setup = do
        observeProperty mpv "filename/no-ext" $ \case
          -- TODO: show different message for non-twitch urls
          -- FIXME: this doesn't handle query params
          Just (treadMaybe -> Just vid) -> load vid
          _ -> unload
        observeProperty mpv "pause" $ atomically . writeTVar active . not
        observeProperty mpv "sub-delay" $ atomically . update . (#delay !~)
        observeEvent mpv "seek" $ atomically $ writeTVar seek True

  contT_ $ withTask_ $ runSettings settings $ runWai app
  contT_ $
    withTask_ $
      forever $ do
        timeout <- registerDelay 1_000_000
        atomically $ do
          guard . isJust . (^. #video) =<< readTVar chatState
          readTVar seek >>= \case
            True -> writeTVar seek False
            False -> do
              guard =<< readTVar active
              guard =<< readTVar timeout
        let unavail (MpvIpcError "property unavailable") = Just ()
            unavail _ = Nothing
        time <- tryJust unavail $ getProperty mpv "playback-time"
        atomically $ update $ #playbackTime !~ (time ^? _Right)
  lift $ runMpv mpv ipcPath `concurrently_` setup
