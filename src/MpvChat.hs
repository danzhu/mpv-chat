{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Builder (byteString)
import Data.Conduit (ConduitT, yield, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Fixed (div')
import Data.Maybe (fromJust)
import Data.Text.IO (putStrLn)
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    addUTCTime,
    nominalDiffTimeToSeconds,
  )
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple
  ( Connection,
    Only (Only, fromOnly),
    query,
    query_,
    withConnection,
  )
import Database.Sqlite.Adapter (JSONField (JSONField))
import Lucid.Base
  ( HtmlT,
    renderBST,
    renderTextT,
    toHtml,
    toHtmlRaw,
  )
import Lucid.Html5
  ( a_,
    alt_,
    class_,
    div_,
    href_,
    img_,
    li_,
    pre_,
    span_,
    src_,
    style_,
    title_,
    ul_,
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
import Network.Request (statusErr)
import qualified Network.Twitch as Tv
import qualified Network.Twitch.Bttv as Bt
import qualified Network.Twitch.Emoticon
import qualified Network.Twitch.Ffz as Fz
import qualified Network.Twitch.Fragment
import Network.URI
  ( parseURI,
    uriAuthority,
  )
import Network.Wai
  ( Application,
    pathInfo,
    responseBuilder,
  )
import Network.Wai.Application.Static
  ( appFile,
    appStatic,
  )
import Network.Wai.Handler.Warp
  ( Port,
    defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setHost,
    setPort,
  )
import Network.Wai.IO
  ( eventData,
    requestBS,
    responseEvents,
    responsePlainStatus,
    responseRedirect,
  )
import Network.Wai.Middleware.StaticRoute
  ( routeAccept,
    routeGet,
    routePost,
  )
import Network.Wai.Monad
  ( Wai,
    WaiApp,
    runWai,
    wai,
  )
import System.FilePath.Posix (takeDirectory)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Environment (getExecutablePath)

newtype EmoteScope = EmoteScope Text
  deriving newtype (IsString)

data Emote = Emote EmoteScope Text

type Emotes = HashMap Text Emote

data EmoteSource
  = DatabaseSource (HashSet Text)
  | UrlSource Emotes

twitchEmoteUrl :: EmoteSource -> Text -> Text
twitchEmoteUrl (DatabaseSource _) id = "emote/" <> id
twitchEmoteUrl (UrlSource _) id = Tv.emoteUrl id

thirdPartyEmote :: EmoteSource -> Text -> Maybe Emote
thirdPartyEmote (DatabaseSource emotes) name
  | member name emotes = Just $ Emote "third-party" ("emote-third-party/" <> name)
thirdPartyEmote (UrlSource emotes) name = lookup name emotes
thirdPartyEmote _ _ = Nothing

data Video = Video
  { id :: Tv.VideoId,
    createdAt :: UTCTime,
    channelId :: Tv.ChannelId,
    emotes :: EmoteSource
  }

data User = User
  { id :: Tv.UserId,
    displayName :: Text,
    name :: Text,
    bio :: Maybe Text
  }

data Badge = Badge
  { _id :: Text
  -- , version :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Highlight
  = NoHighlight
  | NameOnly
  | Highlight
  deriving stock (Eq, Ord)

data Comment = Comment
  { createdAt :: UTCTime,
    commenter :: User,
    fragments :: [Tv.Fragment],
    userColor :: Maybe Text,
    highlight :: Highlight
  }

data ChatState = ChatState
  { _video :: Maybe Video,
    _playbackTime :: Maybe NominalDiffTime,
    _delay :: NominalDiffTime,
    _version :: Int
  }

makeLenses ''ChatState

instance Default ChatState where
  def = ChatState Nothing Nothing 0 0

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

bttv :: EmoteScope -> [Bt.Emote] -> Emotes
bttv s = mapFromList . map emote
  where
    emote Bt.Emote {code, id} = (code, Emote s $ Bt.emoteUrl id)

bttvGlobal :: Bt.Global -> Emotes
bttvGlobal = bttv "bttv global"

bttvChannel :: Bt.Channel -> Emotes
bttvChannel = chan <> shared
  where
    chan = bttv "bttv channel" . Bt.channelEmotes
    shared = bttv "bttv shared" . Bt.sharedEmotes

ffz :: Fz.Channel -> Emotes
ffz = mapFromList . map emote . (Fz.emoticons <=< toList) . Fz.sets
  where
    emote Fz.Emote {name, urls} = (name, Emote "ffz channel" url)
      where
        -- 2 (higher res image) not always available, so fallback to 1
        url = "https:" <> fromJust (asum $ (`lookup` urls) <$> [2, 1])

loadEmotes :: MonadIO m => Tv.ChannelId -> m Emotes
loadEmotes cid =
  liftIO $
    fold
      <$> mapConcurrently
        handle
        [ bttvGlobal <$> Bt.getGlobal,
          bttvChannel <$> Bt.getChannel cid,
          ffz <$> Fz.getChannel cid
        ]
  where
    -- some channels don't have e.g. ffz emotes which results in 404s,
    -- so handle them instead of crashing
    handle :: Monoid a => IO a -> IO a
    handle m =
      tryJust (statusErr (== notFound404)) m >>= \case
        Left _ -> do
          putStrLn "emote loading failed with 404"
          pure mempty
        Right a -> pure a

type Fmt = HtmlT (Reader EmoteSource)

fmtComment :: Comment -> Fmt ()
fmtComment
  Comment
    { fragments,
      commenter = commenter@User {id = commenterId, displayName},
      userColor,
      highlight
    } = do
    li_
      [ class_ $ bool "comment" "comment highlight" $ highlight == Highlight
      ]
      $ do
        a_
          [ class_ "icon",
            style_ $ maybe "" ("background-color: " <>) userColor,
            -- FIXME: user page removed
            href_ $ "/user/" <> tshow commenterId
          ]
          $ fmtUser commenter
        div_ [class_ "message"] do
          unless (highlight == NoHighlight) do
            span_
              [ class_ "name",
                style_ $ maybe "" ("color: " <>) userColor
              ]
              $ toHtml displayName
            ": "
          traverse_ fmtFragment fragments

fmtUser :: User -> Fmt ()
fmtUser User {displayName, name, bio} =
  div_ [class_ "details"] $ do
    span_ [class_ "name"] do
      toHtml displayName
      " ["
      toHtml name
      "]"
    for_ bio $ \b -> div_ $ do
      "Bio: "
      -- FIXME: find out why there are bad chars in twitch response,
      -- which kill the output if not escaped to ascii
      span_ [class_ "bio"] $ toHtml b

fmtFragment :: Tv.Fragment -> Fmt ()
fmtFragment Tv.Fragment {text, emoticon}
  | Just Tv.Emoticon {emoticon_id} <- emoticon = do
    emotes <- ask
    fmtEmote "twitch" text $ twitchEmoteUrl emotes emoticon_id
  | otherwise = fold $ intersperse " " $ fmtWord <$> splitElem ' ' text

fmtWord :: Text -> Fmt ()
fmtWord wor =
  ask >>= \emotes ->
    if
        | Just (Emote ori url) <- thirdPartyEmote emotes wor -> fmtEmote ori wor url
        | Just ('@', nam) <- uncons wor -> do
          -- FIXME: user page removed
          a_ [class_ "mention", href_ $ "/user/" <> nam] $ "@" <> toHtml nam
        | Just (uriAuthority -> Just _) <- parseURI $ toList wor ->
          a_ [class_ "url", href_ wor] $ toHtml wor
        | otherwise -> toHtml wor

fmtEmote :: EmoteScope -> Text -> Text -> Fmt ()
fmtEmote (EmoteScope ori) txt url =
  img_
    [ class_ "emote",
      src_ url,
      title_ $ txt <> " [" <> ori <> "]",
      alt_ txt
    ]

renderComments :: Connection -> ChatState -> HtmlT IO ()
renderComments conn st = case (st ^. video, st ^. playbackTime) of
  (Just Video {id = vid, createdAt = videoUTC, emotes}, Just time) -> do
    chapters <-
      liftIO $
        query
          conn
          "SELECT description FROM chapter \
          \WHERE video_id = ? \
          \AND ? - start_milliseconds BETWEEN 0 AND length_milliseconds \
          \ORDER BY start_milliseconds \
          \LIMIT 1"
          (vid, nominalDiffTimeToSeconds time `div'` 0.001 :: Int)
    pre_ $ do
      toHtml $ formatTime defaultTimeLocale "%h:%2M:%2S" time
      for_ chapters \(Only desc) -> do
        " "
        toHtml (desc :: Text)
      " ["
      toHtml $ tshow $ st ^. delay
      "]"
    ul_ [class_ "comments"] $ do
      comments <-
        liftIO $
          query
            conn
            "SELECT \
            \    c.created_at, c.fragments, c.user_badges, c.user_color, \
            \    u.id, u.display_name, u.name, u.bio, \
            \    f.highlight \
            \FROM comment c \
            \JOIN user u ON u.id = c.commenter \
            \LEFT JOIN follow f ON f.id = u.id \
            \WHERE c.content_id = ? AND c.created_at < ? \
            \ORDER BY c.created_at DESC \
            \LIMIT 500"
            (vid, addUTCTime time videoUTC)
      for_
        comments
        \( createdAt,
           JSONField fragments,
           JSONField badges,
           userColor,
           uid,
           displayName,
           name,
           bio,
           highlight
           ) -> do
            let u = User {id = uid, displayName, name, bio}
                hl = maybe NoHighlight (bool NameOnly Highlight) highlight
                hlMod =
                  bool NoHighlight NameOnly $
                    any (\Badge {_id} -> _id == "moderator") (badges :: [Badge])
                c =
                  Comment
                    { createdAt,
                      commenter = u,
                      fragments,
                      userColor,
                      highlight = max hl hlMod
                    }
                html = runReader (renderBST $ fmtComment c) emotes
            toHtmlRaw html
  _ -> pre_ "idle"

err :: Status -> Application
err stat _ res = res $ responsePlainStatus stat []

page :: ConduitT () View IO () -> WaiApp
page vs =
  wai $
    routeGet err $
      routeAccept
        err
        [ ("text/html", appFile err "res/page.html"),
          ("text/event-stream", str)
        ]
  where
    str _ res = res $ responseEvents $ vs .| C.map enc
    enc v = def {eventData = encode v}

post :: Wai () -> WaiApp
post app = wai . routePost err . runWai $ do
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
            let new = st ^. version
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
          ["emote", id] -> wai $
            routeGet err $ \req res -> do
              query conn "SELECT data FROM emote WHERE id = ?" (Only id) >>= \case
                [Only bs] -> res $ responseBuilder ok200 [] $ byteString bs
                _ -> err notFound404 req res
          ["emote-third-party", name] -> wai $
            routeGet err $ \req res -> do
              query conn "SELECT data FROM emote_third_party WHERE name = ?" (Only name) >>= \case
                [Only bs] -> res $ responseBuilder ok200 [] $ byteString bs
                _ -> err notFound404 req res
          -- actions
          ["loadfile"] -> post $ do
            url <- join $ asks requestBS
            void $ liftIO $ loadfile mpv $ decodeUtf8 url
          -- docs
          ["doc"] -> pure $ responseRedirect "/doc/all/index.html"
          "doc" : _ -> wai $ appStatic err installPath
          -- static
          _ -> wai $ appStatic err "public"
      update f = modifyTVar' chatState $ (version %~ succ) . f
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
          update $ video ?~ Video {id = vid, createdAt, channelId, emotes}
          writeTVar seek True
      unload =
        startTask taskLoad $
          atomically $ update $ video .~ Nothing
      setup = do
        observeProperty mpv "filename/no-ext" $ \case
          Nothing -> unload
          Just n -> case Tv.parseVideoId n of
            -- TODO: show different message for non-twitch urls
            Left _ -> unload
            Right vid -> load vid
        observeProperty mpv "pause" $ atomically . writeTVar active . not
        observeProperty mpv "sub-delay" $ \d ->
          atomically $ update $ delay .~ d
        observeEvent mpv "seek" $ atomically $ writeTVar seek True

  contT_ $ withTask_ $ runSettings settings $ runWai app
  contT_ $
    withTask_ $
      forever $ do
        timeout <- registerDelay 1_000_000
        atomically $ do
          guard . isJust . (^. video) =<< readTVar chatState
          readTVar seek >>= \case
            True -> writeTVar seek False
            False -> do
              guard =<< readTVar active
              guard =<< readTVar timeout
        let unavail (MpvIpcError "property unavailable") = Just ()
            unavail _ = Nothing
        tryJust unavail (getProperty mpv "playback-time") >>= \case
          Left () -> pure ()
          Right t -> atomically $ update $ playbackTime .~ t
  lift $ runMpv mpv ipcPath `concurrently_` setup
