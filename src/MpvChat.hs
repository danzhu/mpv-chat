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
import MpvChat.Data (ChatState (ChatState), View)
import qualified MpvChat.Data
import MpvChat.Database (loadEmote, loadFile, loadVideo)
import MpvChat.Videos (renderVideos)
import Network.HTTP.Types.Status
  ( Status,
    notFound404,
    ok200,
  )
import Network.Mpv
  ( MpvError (MpvIpcError),
    clientName,
    getProperty,
    loadfile,
    observeEvent_,
    observeProperty,
    waitMpv,
    withMpv,
  )
import Network.Twitch (VideoId)
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
import Text.Regex.TDFA ((=~~))
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

parseVideoId :: Text -> Maybe VideoId
parseVideoId = treadMaybe <=< (=~~ ("[0-9]{10}" :: String))

runMpvChat :: Config -> IO ()
runMpvChat Config {ipcPath, port} = evalContT $ do
  conn <- ContT $ withConnection "twitch.db"
  mpv <- ContT $ withMpv ipcPath
  ipcName <- liftIO $ clientName mpv
  liftIO $ putStrLn $ "connected to mpv with name " <> ipcName

  chatState <- newTVarIO def
  active <- newTVarIO False
  seek <- newTVarIO False

  exePath <- getExecutablePath
  let installPath = takeDirectory $ takeDirectory exePath

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
          view <- case st of
            ChatState
              { video = Just video,
                playbackTime = Just playbackTime,
                delay
              } -> do
                liftIO $ renderChat conn video playbackTime delay uid
            _ -> liftIO $ renderVideos conn
          yield view
          -- update at most once per 100ms
          threadDelay 100_000
      app =
        asks pathInfo >>= \case
          -- chat messages
          [] -> page $ messages Nothing
          ["user", treadMaybe -> Just uid] -> page $ messages (Just uid)
          ["emote", id] ->
            routeGet err do
              liftIO (loadEmote conn id) >>= \case
                Just bs -> pure $ responseBuilder ok200 [] $ byteString bs
                Nothing -> err notFound404
          ["file", name] ->
            routeGet err do
              liftIO (loadFile conn name) >>= \case
                Just bs -> pure $ responseBuilder ok200 [] $ byteString bs
                Nothing -> err notFound404
          -- videos
          ["videos"] -> page $ do
            videos <- liftIO $ renderVideos conn
            yield videos
            forever $ threadDelay maxBound
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
        v <- for vid $ loadVideo conn
        atomically $ update $ (#video !~ v) . (#playbackTime !~ Nothing)
      setup = do
        -- TODO: show different message for non-twitch urls
        observeProperty mpv #"filename/no-ext" $ load . (parseVideoId =<<)
        observeProperty mpv #pause $ atomically . writeTVar active . not
        observeProperty mpv #"sub-delay" $ atomically . update . (#delay !~)
        observeEvent_ mpv #seek $ atomically $ writeTVar seek True

  contT_ $ withTask_ $ runSettings settings $ runWai app
  contT_ $
    withTask_ $
      forever $ do
        timeout <- registerDelay 1_000_000
        atomically $ do
          st <- readTVar chatState
          -- if no video, there's no playback time, so block
          guard $ isJust $ st ^. #video
          readTVar seek >>= \case
            -- if seeking, immediately continue for low latency
            True -> writeTVar seek False
            False -> do
              -- otherwise, wait for timeout first
              guard =<< readTVar timeout
              -- if we have playback time already,
              -- only continue when not paused
              when (isJust $ st ^. #playbackTime) $
                guard =<< readTVar active
        let unavail (MpvIpcError "property unavailable") = Just ()
            unavail _ = Nothing
        time <- tryJust unavail $ getProperty mpv #"playback-time"
        atomically $ update $ #playbackTime !~ (time ^? _Right)
  liftIO setup
  waitMpv mpv
