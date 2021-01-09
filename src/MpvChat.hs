{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module MpvChat
  ( Config (..),
    Tv.Auth (..),
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
import Data.Conduit (ConduitT, runConduit, yield, (.|))
import qualified Data.Conduit.Combinators as C
import Data.Maybe (fromJust)
import qualified Data.SeekBuffer as SB
import Data.Text.IO (putStrLn)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Lucid.Base
  ( Html,
    HtmlT,
    renderBST,
    renderText,
    toHtml,
    toHtmlRaw,
  )
import Lucid.Html5
  ( a_,
    alt_,
    button_,
    class_,
    data_,
    div_,
    h1_,
    h3_,
    href_,
    img_,
    li_,
    p_,
    pre_,
    span_,
    src_,
    style_,
    title_,
    ul_,
  )
import Network.HTTP.Types.Status
  ( Status,
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
import qualified Network.Twitch.Bttv as Bt
import qualified Network.Twitch.Channel
import qualified Network.Twitch.Comment
import qualified Network.Twitch.Emoticon
import qualified Network.Twitch.Ffz as Fz
import qualified Network.Twitch.Fragment
import qualified Network.Twitch.Message
import qualified Network.Twitch.User
import qualified Network.Twitch.Video
import Network.URI
  ( parseURI,
    uriAuthority,
  )
import Network.Wai
  ( Application,
    pathInfo,
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

newtype Scope = Scope Text
  deriving newtype (IsString)

data Emote = Emote Scope Text

type Emotes = HashMap Text Emote

type Highlights = HashSet Text

type Mentions = [Text]

type Fmt = HtmlT (RWS (Emotes, Highlights) Mentions ())

data Comment = Comment
  { time :: NominalDiffTime,
    html :: LByteString,
    user :: Text,
    display_user :: Text,
    mentions :: Mentions
  }

data Play = Play
  { _comments :: SB.SeekBuffer Comment,
    _done :: Bool,
    _current :: NominalDiffTime,
    _latest :: NominalDiffTime
  }

makeLenses ''Play

instance Default Play where
  def = Play SB.empty False 0 0

data ChatState = ChatState
  { _play :: Maybe Play,
    _delay :: NominalDiffTime,
    _version :: Int
  }

makeLenses ''ChatState

instance Default ChatState where
  def = ChatState Nothing 0 0

data View = View
  { title :: Text,
    content :: LText
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data Config = Config
  { ipcPath :: FilePath,
    auth :: Tv.Auth,
    port :: Port,
    highlights :: Highlights
  }
  deriving stock (Show)

bttv :: Scope -> [Bt.Emote] -> Emotes
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
        url = fromJust $ asum $ (`lookup` urls) <$> [2, 1]

loadEmotes :: MonadIO m => Tv.ChannelId -> m Emotes
loadEmotes cid =
  liftIO $
    fold
      <$> mapConcurrently
        identity
        [ bttvGlobal <$> Bt.getGlobal,
          bttvChannel <$> Bt.getChannel cid,
          ffz <$> Fz.getChannel cid
        ]

fmtComment :: Tv.Comment -> Fmt ()
fmtComment
  Tv.Comment
    { message = Tv.Message {user_color, fragments},
      commenter = usr@Tv.User {display_name, name}
    } = do
    hls <- asks snd
    let hl = member name hls
    li_
      [ class_ $ bool "comment" "comment highlight" hl
      ]
      $ do
        a_
          [ class_ "icon",
            style_ $ maybe "" ("background-color: " <>) user_color,
            href_ $ "/user/" <> name
          ]
          $ fmtUser usr
        div_ [class_ "message"] do
          when hl do
            span_
              [ class_ "name",
                style_ $ maybe "" ("color: " <>) user_color
              ]
              $ toHtml display_name
            ": "
          traverse_ fmtFragment fragments

fmtUser :: Tv.User -> Fmt ()
fmtUser Tv.User {display_name, bio} =
  div_ [class_ "details"] $ do
    span_ [class_ "name"] $ toHtml display_name
    for_ bio $ \b -> div_ $ do
      "Bio: "
      -- FIXME: find out why there are bad chars in twitch response,
      -- which kill the output if not escaped to ascii
      span_ [class_ "bio"] $ toHtml $ show b

fmtFragment :: Tv.Fragment -> Fmt ()
fmtFragment Tv.Fragment {text, emoticon}
  | Just Tv.Emoticon {emoticon_id} <- emoticon =
    fmtEmote "twitch" text $ Tv.emoteUrl emoticon_id
  | otherwise = fold $ intersperse " " $ fmtWord <$> splitElem ' ' text

fmtWord :: Text -> Fmt ()
fmtWord wor =
  asks fst >>= \emotes ->
    if
        | Just (Emote ori url) <- lookup wor emotes -> fmtEmote ori wor url
        | Just ('@', nam) <- uncons wor -> do
          tell [nam]
          a_ [class_ "mention", href_ $ "/user/" <> nam] $ "@" <> toHtml nam
        | Just (uriAuthority -> Just _) <- parseURI $ toList wor ->
          a_ [class_ "url", href_ wor] $ toHtml wor
        | otherwise -> toHtml wor

fmtEmote :: Scope -> Text -> Text -> Fmt ()
fmtEmote (Scope ori) txt url =
  img_
    [ class_ "emote",
      src_ $ "https:" <> url,
      title_ $ txt <> " [" <> ori <> "]",
      alt_ txt
    ]

format :: Emotes -> Highlights -> Tv.Comment -> Comment
format
  es
  hls
  c@Tv.Comment
    { commenter = Tv.User {name = user, display_name = display_user},
      content_offset_seconds = time
    } =
    Comment {html, time, user, display_user, mentions}
    where
      (html, mentions) = evalRWS (renderBST $ fmtComment c) (es, hls) ()

renderTime :: NominalDiffTime -> Html ()
renderTime = toHtml . formatTime defaultTimeLocale "%h:%2M:%2S"

renderComments :: ([Comment] -> [Comment]) -> ChatState -> Html ()
renderComments f st = case st ^. play of
  Nothing -> pre_ "idle"
  Just (Play cms don cur lat) -> do
    pre_ $ do
      renderTime cur
      " ["
      toHtml $ tshow $ st ^. delay
      "] / "
      renderTime lat
      unless don " ..."
    if not don && null (SB.future cms)
      then pre_ "buffering..."
      else
        ul_ [class_ "comments"] $
          toHtmlRaw $ foldMap html $ f $ SB.past cms

renderVideos :: Tv.User -> [Tv.Video] -> Html ()
renderVideos Tv.User {display_name, bio} vs = do
  h1_ $ toHtml display_name
  for_ bio $ p_ [class_ "bio"] . toHtml . tshow
  ul_ [class_ "videos"] $
    for_ vs $
      \Tv.Video
         { title,
           views,
           url,
           published_at,
           game,
           length = len,
           preview = Tv.Images {large}
         } ->
          li_ [class_ "video"] $ do
            img_ [class_ "preview", src_ large]
            div_ [class_ "meta"] $ do
              div_ [class_ "info"] $ do
                h3_ [class_ "title", title_ title] $ toHtml title
                p_ $ do
                  toHtml $ formatTime defaultTimeLocale "%F" published_at
                  ", "
                  renderTime len
                  ", "
                  toHtml $ tshow views
                  " views"
                for_ game $ p_ [class_ "game"] . toHtml
              div_ [class_ "actions"] $
                button_
                  [ class_ "load",
                    data_ "post" "/loadfile",
                    data_ "body" url
                  ]
                  "|>"

reseek :: ChatState -> ChatState
reseek st = play . _Just %~ upd $ st
  where
    upd p = comments %~ SB.seek adv $ p
      where
        adv c = time c < t
        t = p ^. current - st ^. delay

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

follows :: MonadIO m => Highlights -> ConduitT i View m ()
follows hls = do
  yield $
    View "Follows" $
      renderText $
        ul_ [class_ "follows"] $
          for_ hls $ \h ->
            li_ [class_ "follow"] $
              a_ [href_ $ "/channel/" <> h] $ toHtml h
  threadDelay maxBound

videos :: MonadIO m => Tv.Auth -> Text -> ConduitT i View m ()
videos auth name = do
  liftIO (Tv.getUserByName auth name) >>= \case
    Nothing ->
      yield $
        View "Not Found" $
          renderText $
            h1_ "user not found"
    Just user@Tv.User {_id, display_name} -> do
      let cid = Tv.userChannel _id
      vs <-
        liftIO $
          runConduit $
            Tv.getChannelVideos auth cid
              .| C.concat
              .| C.sinkList
      yield $ View display_name $ renderText $ renderVideos user vs
  threadDelay maxBound

runMpvChat :: Config -> IO ()
runMpvChat Config {ipcPath, auth, port, highlights} = evalContT $ do
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
      messages f = do
        ver <- newTVarIO (-1)
        forever $ do
          st <- atomically $ do
            st <- readTVar chatState
            cur <- readTVar ver
            let new = st ^. version
            guard $ cur < new
            writeTVar ver new
            pure st
          yield $ View "Chat" $ renderText $ renderComments f st
          -- update at most once per 100ms
          threadDelay 100_000
      app =
        asks pathInfo >>= \case
          -- chat messages
          [] -> page $ messages $ take 500
          ["user", usr] -> page $ messages $ take 50 . filter p
            where
              p c = user c == usr || display_user c == usr
          ["follows"] -> page $ follows highlights
          ["channel", chan] -> page $ videos auth chan
          -- actions
          ["loadfile"] -> post $ do
            url <- join $ asks requestBS
            void $ liftIO $ loadfile mpv $ decodeUtf8 url
          -- docs
          ["doc"] -> pure $ responseRedirect "/doc/all/index.html"
          "doc" : _ -> wai $ appStatic err installPath
          -- static
          _ -> wai $ appStatic err "public"
      update f = modifyTVar' chatState $ (version %~ succ) . reseek . f
      append cs =
        atomically $
          update $
            play . _Just
              %~ (latest %~ \t -> maybe t time $ cs ^? _last)
                . (comments %~ SB.append cs)
      load vid = startTask taskLoad $ do
        atomically $ do
          update $ play ?~ def
          writeTVar seek True
        Tv.Video {channel = Tv.Channel {_id = cid}} <-
          Tv.getVideo auth vid
        emotes <- loadEmotes cid
        runConduit $
          Tv.getVideoComments auth vid
            .| C.mapE (format emotes highlights)
            .| C.mapM_ append
        atomically $ update $ play . _Just . done .~ True
      unload =
        startTask taskLoad $
          atomically $ update $ play .~ Nothing
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
          guard . isJust . (^. play) =<< readTVar chatState
          readTVar seek >>= \case
            True -> writeTVar seek False
            False -> do
              guard =<< readTVar active
              guard =<< readTVar timeout
        let unavail (MpvIpcError "property unavailable") = Just ()
            unavail _ = Nothing
            upd t = atomically $ update $ play . _Just . current .~ t
        either pure upd =<< tryJust unavail (getProperty mpv "playback-time")
  lift $ runMpv mpv ipcPath `concurrently_` setup
