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
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Builder (byteString)
import Data.Conduit (ConduitT, yield, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Text.IO (putStrLn)
import Database.SQLite.Simple
  ( Only (Only, fromOnly),
    query,
    query_,
    withConnection,
  )
import Lucid.Base (renderTextT)
import MpvChat.Chat (renderComments)
import MpvChat.Data (Video (Video))
import qualified MpvChat.Data
import MpvChat.Emote
  ( EmoteSource (DatabaseSource, UrlSource),
    loadEmotes,
  )
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
import qualified Network.Twitch as Tv
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
    eventData,
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

data View = View
  { title :: Text,
    content :: LText
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data Config = Config
  { ipcPath :: FilePath,
    port :: Port,
    online :: Bool
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
    enc v = def {eventData = encode v}

post :: Wai () -> WaiApp
post app = routePost err $ do
  app
  pure $ responsePlainStatus ok200 []

runMpvChat :: Config -> IO ()
runMpvChat Config {ipcPath, port, online} = evalContT $ do
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
      messages = do
        ver <- newTVarIO (-1)
        forever $ do
          st <- atomically $ do
            st <- readTVar chatState
            cur <- readTVar ver
            let new = st ^. #version
            guard $ cur < new
            writeTVar ver new
            pure st
          html <- liftIO $ renderTextT $ renderComments conn st
          yield $ View "Chat" html
          -- update at most once per 100ms
          threadDelay 100_000
      app =
        asks pathInfo >>= \case
          -- chat messages
          [] -> page messages
          ["emote", id] ->
            routeGet err do
              liftIO (query conn "SELECT data FROM emote WHERE id = ?" (Only id)) >>= \case
                [Only bs] -> pure $ responseBuilder ok200 [] $ byteString bs
                _ -> err notFound404
          ["emote-third-party", name] ->
            routeGet err do
              liftIO (query conn "SELECT data FROM emote_third_party WHERE name = ?" (Only name)) >>= \case
                [Only bs] -> pure $ responseBuilder ok200 [] $ byteString bs
                _ -> err notFound404
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
        [(createdAt, channelId)] <-
          query
            conn
            "SELECT created_at, channel_id FROM video WHERE id = ?"
            (Only vid)
        emotes <-
          if online
            then UrlSource <$> loadEmotes channelId
            else
              DatabaseSource . setFromList . map fromOnly
                <$> query_ conn "SELECT name FROM emote_third_party"
        atomically $ do
          -- TODO: fix glitch where playback-time from last video is used before
          -- the new video is loaded
          update $ #video ?!~ Video {id = vid, createdAt, channelId, emotes}
          writeTVar seek True
      unload =
        startTask taskLoad $
          atomically $ update $ #video !~ Nothing
      setup = do
        observeProperty mpv "filename/no-ext" $ \case
          Nothing -> unload
          Just n -> case Tv.parseVideoId n of
            -- TODO: show different message for non-twitch urls
            Left _ -> unload
            Right vid -> load vid
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
        tryJust unavail (getProperty mpv "playback-time") >>= \case
          Left () -> pure ()
          Right t -> atomically $ update $ #playbackTime !~ t
  lift $ runMpv mpv ipcPath `concurrently_` setup
