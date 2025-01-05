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
import Data.ByteString.Builder (byteString, intDec)
import Data.Conduit (ConduitT, yield, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Text.IO (putStrLn)
import Database.SQLite.Simple (withConnection)
import MpvChat.Chat (renderChat)
import MpvChat.Data (ChatState (ChatState), View)
import qualified MpvChat.Data
import MpvChat.Database (loadEmote, loadFile, loadVideo)
import MpvChat.Videos (renderVideos)
import Network.HTTP.Types (Status (statusCode, statusMessage), StdMethod (GET, POST))
import Network.HTTP.Types.Status
  ( badRequest400,
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
    catchWai,
    requestBS,
    runWai,
    throwWai,
  )
import Network.Wai.Route
  ( Event (data_),
    Route,
    WithStatus (WithStatus),
    eventsRoute,
    fileRoute,
    redirect,
    route,
    route',
    routeAny',
    runRoute,
    staticRoute,
  )
import System.FilePath (takeDirectory, (</>))
import Text.Regex.TDFA ((=~~))
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (listDirectory, makeAbsolute)
import UnliftIO.Environment (getExecutablePath)

data Config = Config
  { ipcPath :: FilePath,
    port :: Port
  }
  deriving stock (Show)

page :: ConduitT () View IO () -> Route ()
page vs = do
  fileRoute "res/page.html"
  eventsRoute $ vs .| C.map enc
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

      app :: [Text] -> Route ()
      -- chat messages
      app [] = page $ messages Nothing
      app ["user", treadMaybe -> Just uid] = page $ messages (Just uid)
      app ["emote", id] =
        liftIO (loadEmote conn id) >>= traverse_ (route' GET Nothing . pure)
      app ["file", name] =
        liftIO (loadFile conn name) >>= traverse_ (route' GET Nothing . pure)
      -- videos
      app ["videos"] = page do
        videos <- liftIO $ renderVideos conn
        yield videos
        forever $ threadDelay maxBound
      -- actions
      app ["loadfile"] = route POST do
        body <- join $ asks requestBS
        case treadMaybe $ decodeUtf8 body of
          Nothing -> throwWai badRequest400 "invalid request body"
          Just id -> do
            path <- videoPath id
            void $ liftIO $ loadfile mpv path
      -- docs
      app ["doc"] = redirect "/doc/all/index.html"
      app path@("doc" : _) = staticRoute installPath path
      -- static
      app path = staticRoute "public" path

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

  contT_ $ withTask_ $ runSettings settings $ runWai do
    path <- asks pathInfo
    catchWai (runRoute $ app path) \(ErrorResponse s msg) -> runRoute do
      -- statusMessage is bytestring even though it's plain text,
      -- so override content type to text/plain
      routeAny' (Just "text/plain; charset=utf-8") do
        let body =
              intDec (statusCode s)
                <> " "
                <> byteString (statusMessage s)
                <> "\n"
                <> encodeUtf8Builder msg
        pure $ WithStatus s body

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
