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
  ( badRequest400,
    noContent204,
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
  ( ErrorResponse (ErrorResponse),
    WaiApp,
    appFile,
    appStatic,
    data_,
    requestBS,
    responseEvents,
    responseRedirect,
    routeAccept,
    routeGet,
    routePost,
    runWai,
    throwWai,
  )
import System.FilePath (takeDirectory, (</>))
import Text.Regex.TDFA ((=~~))
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (listDirectory, makeAbsolute)
import UnliftIO.Environment (getExecutablePath)
import Network.HTTP.Types.Header (hContentType)

data Config = Config
  { ipcPath :: FilePath,
    port :: Port
  }
  deriving stock (Show)

page :: ConduitT () View IO () -> WaiApp
page vs =
  routeGet $
    routeAccept
      [ ("text/html", appFile "res/page.html"),
        ("text/event-stream", pure $ responseEvents $ vs .| C.map enc)
      ]
  where
    enc v = def {data_ = encode v}

parseVideoId :: Text -> Maybe VideoId
parseVideoId = treadMaybe <=< (=~~ ("[0-9]{10}" :: String))

videoPath :: (MonadIO m) => VideoId -> m Text
videoPath vid = do
  dir <- makeAbsolute "vod"
  names <- listDirectory dir
  case find (\name -> parseVideoId (fromList name) == Just vid) names of
    Just path -> pure $ fromList $ dir </> path
    Nothing -> pure $ "https://www.twitch.tv/videos/" <> tshow vid

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
            routeGet do
              liftIO (loadEmote conn id) >>= \case
                Just bs -> pure $ responseBuilder ok200 [] $ byteString bs
                Nothing -> throwWai notFound404 ""
          ["file", name] ->
            routeGet do
              liftIO (loadFile conn name) >>= \case
                Just bs -> pure $ responseBuilder ok200 [] $ byteString bs
                Nothing -> throwWai notFound404 ""
          -- videos
          ["videos"] -> page $ do
            videos <- liftIO $ renderVideos conn
            yield videos
            forever $ threadDelay maxBound
          -- actions
          ["loadfile"] -> routePost $ do
            body <- join $ asks requestBS
            case treadMaybe $ decodeUtf8 body of
              Nothing -> throwWai badRequest400 "invalid request body"
              Just id -> do
                path <- videoPath id
                void $ liftIO $ loadfile mpv path
                pure $ responseBuilder noContent204 [] ""
          -- docs
          ["doc"] -> pure $ responseRedirect "/doc/all/index.html"
          "doc" : _ -> appStatic installPath
          -- static
          _ -> appStatic "public"
      update f = modifyTVar' chatState $ (#version %!~ succ) . f
      load vid = startTask taskLoad $ do
        -- TODO: inform user for non-twitch videos,
        -- and twitch videos not yet imported
        v <- join <$> for vid (loadVideo conn)
        atomically $ update $ (#video !~ v) . (#playbackTime !~ Nothing)
      setup = do
        observeProperty mpv #filename $ load . (parseVideoId =<<)
        observeProperty mpv #pause $ atomically . writeTVar active . not
        observeProperty mpv #"sub-delay" $ atomically . update . (#delay !~)
        observeEvent_ mpv #seek $ atomically $ writeTVar seek True

  contT_ $ withTask_ $ runSettings settings $ \req resp -> do
    try (runWai app req resp) >>= \case
      Right r -> pure r
      Left (ErrorResponse s m) -> do
        let hs = [(hContentType, "text/plain; charset=utf-8")]
            b = byteString (encodeUtf8 m)
        resp $ responseBuilder s hs b

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
