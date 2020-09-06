{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module MpvChat
  ( Config(..)
  , Tv.Auth(..)
  , runMpvChat
  ) where

import           Control.Concurrent.Task        ( startTask
                                                , withEmptyTask
                                                , withTask_
                                                )
import           Control.Monad.ContT            ( contT_ )
import           Control.Monad.RWS              ( asks
                                                , tell
                                                )
import           Data.Aeson                     ( encode )
import qualified Data.Conduit.Combinators      as C
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromJust )
import qualified Data.SeekBuffer               as SB
import           Data.Text.IO                   ( putStrLn )
import           Lucid.Base                     ( Html
                                                , HtmlT
                                                , renderBST
                                                , renderText
                                                , toHtml
                                                , toHtmlRaw
                                                )
import           Lucid.Html5                    ( a_
                                                , button_
                                                , class_
                                                , data_
                                                , div_
                                                , h3_
                                                , href_
                                                , img_
                                                , li_
                                                , p_
                                                , pre_
                                                , span_
                                                , src_
                                                , style_
                                                , title_
                                                , ul_
                                                )
import           MpvChat.Prelude
import           Network.HTTP.Types.Status      ( Status
                                                , ok200
                                                )
import           Network.Mpv                    ( MpvError(MpvIpcError)
                                                , getProperty
                                                , loadfile
                                                , newMpvClient
                                                , observeEvent
                                                , observeProperty
                                                , runMpv
                                                )
import qualified Network.Twitch                as Tv
import qualified Network.Twitch.Bttv           as Bt
import qualified Network.Twitch.Channel
import qualified Network.Twitch.Comment
import qualified Network.Twitch.Emoticon
import qualified Network.Twitch.Ffz            as Fz
import qualified Network.Twitch.Fragment
import qualified Network.Twitch.Message
import qualified Network.Twitch.User
import qualified Network.Twitch.Video
import           Network.URI                    ( parseURI
                                                , uriAuthority
                                                )
import           Network.Wai                    ( Application
                                                , pathInfo
                                                )
import           Network.Wai.Application.Static ( appFile
                                                , appStatic
                                                )
import           Network.Wai.Handler.Warp       ( Port
                                                , defaultSettings
                                                , runSettings
                                                , setBeforeMainLoop
                                                , setHost
                                                , setPort
                                                )
import           Network.Wai.IO                 ( eventData
                                                , requestBS
                                                , responseEvents
                                                , responsePlainStatus
                                                , responseRedirect
                                                )
import           Network.Wai.Middleware.StaticRoute
                                                ( routeAccept
                                                , routeGet
                                                , routePost
                                                )
import           Network.Wai.Monad              ( WaiApp
                                                , runWai
                                                , wai
                                                )
import           System.FilePath.Posix          ( takeDirectory )
import           Text.Printf                    ( printf )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Environment           ( getExecutablePath )

type Scope = Text
type Emote = (Scope, Text)
type Emotes = HashMap Text Emote
type Highlights = HashSet Text
type Mentions = [Text]
type Fmt = HtmlT (RWS (Emotes, Highlights) Mentions ())

data Comment = Comment
  { time :: Scientific
  , html :: LByteString
  , user :: Text
  , display_user :: Text
  , mentions :: Mentions
  }

data Play = Play
  { _comments :: SB.SeekBuffer Comment
  , _done :: Bool
  , _current :: Scientific
  , _latest :: Scientific
  }

makeLenses ''Play

instance Default Play where
  def = Play SB.empty False 0 0

data ChatState = ChatState
  { _play :: Maybe Play
  , _delay :: Scientific
  }

makeLenses ''ChatState

instance Default ChatState where
  def = ChatState Nothing 0

data View = View
  { title :: Text
  , content :: LText
  }
  deriving stock (Generic, Show)
  deriving anyclass ToJSON

data Config = Config
  { ipcPath :: FilePath
  , auth :: Tv.Auth
  , port :: Port
  , highlights :: Highlights
  }
  deriving stock Show

bttv :: Scope -> [Bt.Emote] -> Emotes
bttv s = mapFromList . map emote where
  emote e = (Bt.code e, ("bttv " <> s, Bt.emoteUrl $ Bt.id e))

bttvGlobal :: Bt.Global -> Emotes
bttvGlobal = bttv "global"

bttvChannel :: Bt.Channel -> Emotes
bttvChannel = chan <> shared where
  chan = bttv "channel" . Bt.channelEmotes
  shared = bttv "channel shared" . Bt.sharedEmotes

ffz :: Fz.Channel -> Emotes
ffz = mapFromList . map emote . (Fz.emoticons <=< toList . Fz.sets) where
  emote e = (Fz.name e, ("ffz channel", url)) where
    -- 2 (higher res image) not always available, so fallback to 1
    url = fromJust $ asum $ (`lookup` Fz.urls e) <$> [2, 1]

loadEmotes :: MonadIO m => Tv.ChannelId -> m Emotes
loadEmotes cid = liftIO $ fold <$> mapConcurrently identity
  [ bttvGlobal <$> Bt.getGlobal
  , bttvChannel <$> Bt.getChannel cid
  , ffz <$> Fz.getChannel cid
  ]

fmtComment :: Tv.Comment -> Fmt ()
fmtComment Tv.Comment
  { message = Tv.Message { user_color, fragments }
  , commenter = usr@Tv.User { name }
  } = do
  hls <- asks snd
  li_
    [ class_ $ bool "comment" "comment highlight" $ member name hls
    ] $ do
    a_
      [ class_ "icon"
      , style_ $ maybe "" ("background-color: " <>) user_color
      , href_ $ "/user/" <> name
      ] $
      fmtUser usr
    div_ [class_ "message"] $
      traverse_ fmtFragment fragments

fmtUser :: Tv.User -> Fmt ()
fmtUser Tv.User { display_name, bio } =
  div_ [class_ "details"] $ do
    span_ [class_ "name"] $ toHtml display_name
    for_ bio $ \b -> div_ $ do
      "Bio: "
      -- FIXME: find out why there are bad chars in twitch response,
      -- which kill the output if not escaped to ascii
      span_ [class_ "bio"] $ toHtml $ show b

fmtFragment :: Tv.Fragment -> Fmt ()
fmtFragment Tv.Fragment { text, emoticon }
  | Just Tv.Emoticon { emoticon_id } <- emoticon =
      fmtEmote "twitch" text $ Tv.emoteUrl emoticon_id
  | otherwise = fold $ intersperse " " $ fmtWord <$> splitElem ' ' text

fmtWord :: Text -> Fmt ()
fmtWord wor = asks fst >>= \emotes -> if
  | Just (ori, url) <- lookup wor emotes -> fmtEmote ori wor url
  | Just ('@', nam) <- uncons wor -> do
      tell [nam]
      a_ [class_ "mention", href_ $ "/user/" <> nam] $ "@" <> toHtml nam
  | Just (uriAuthority -> Just _) <- parseURI $ toList wor ->
      a_ [class_ "url", href_ wor] $ toHtml wor
  | otherwise -> toHtml wor

fmtEmote :: Text -> Text -> Text -> Fmt ()
fmtEmote ori txt url = img_
  [ class_ "emote"
  , src_ $ "https:" <> url
  , title_ $ txt <> " [" <> ori <> "]"
  ]

format :: Emotes -> Highlights -> Tv.Comment -> Comment
format es hls c@Tv.Comment
  { commenter = Tv.User { name = user, display_name = display_user }
  , content_offset_seconds = time
  } =
  Comment { html, time, user, display_user, mentions }
  where (html, mentions) = evalRWS (renderBST $ fmtComment c) (es, hls) ()

renderComments :: ([Comment] -> [Comment]) -> ChatState -> Html ()
renderComments f st = case st ^. play of
  Nothing -> pre_ "idle"
  Just (Play cms don cur lat) -> do
    let int = round :: Scientific -> Int
    pre_ $ toHtml $ fold
      [ show $ int cur
      , printf " [%+d]" $ int $ st ^. delay
      , " / "
      , show $ int lat
      , bool " ..." "" don
      ]
    if not don && null (SB.future cms)
      then pre_ "buffering..."
      else ul_ [class_ "comments"] $
        toHtmlRaw $ foldMap html $ f $ SB.past cms

renderVideos :: [Tv.Video] -> Html ()
renderVideos vs =
  ul_ [class_ "videos"] $
    for_ vs $ \Tv.Video
      { title
      , views
      , url
      , game
      , length = len
      , preview = Tv.Images { large }
      } ->
      li_ [class_ "video"] $ do
        img_ [class_ "preview", src_ large]
        div_ [class_ "meta"] $ do
          div_ [class_ "info"] $ do
            h3_ [class_ "title"] $ toHtml title
            p_ $ toHtml $ tshow len <> " secs, " <> tshow views <> " views"
            for_ game $ p_ [class_ "game"] . toHtml
          div_ [class_ "actions"] $
            button_ [class_ "load", data_ "post" "/loadfile", data_ "body" url] "|>"

reseek :: ChatState -> ChatState
reseek st = play . _Just %~ upd $ st where
  upd p = comments %~ SB.seek adv $ p where
    adv c = time c < t
    t = p ^. current - st ^. delay

err :: Status -> Application
err stat _ res = res $ responsePlainStatus stat []

page :: ConduitT () View IO () -> WaiApp
page vs = wai $ routeGet err $ routeAccept err
  [ ("text/html", appFile err "res/page.html")
  , ("text/event-stream", str)
  ]
  where
    str _ res = res $ responseEvents $ vs .| C.map enc
    enc v = def { eventData = encode v }

videos :: MonadIO m => Tv.Auth -> Text -> ConduitT i View m ()
videos auth chan = do
  h <- liftIO $ case Tv.parseChannelId chan of
    Left e -> pure $ "invalid channel id: " <> toHtml e
    Right cid -> do
      vs <- runConduit $
        Tv.getChannelVideos auth cid
        .| C.concat
        .| C.sinkList
      pure $ renderVideos vs
  yield $ View chan $ renderText h
  threadDelay maxBound

runMpvChat :: Config -> IO ()
runMpvChat Config { ipcPath, auth, port, highlights } = evalContT $ do
  chatState <- newTVarIO def
  active <- newTVarIO False
  seek <- newTVarIO False
  redraw <- newBroadcastTChanIO

  exePath <- getExecutablePath
  let installPath = takeDirectory $ takeDirectory exePath

  mpv <- newMpvClient
  taskLoad <- ContT withEmptyTask

  let entry = putStrLn $ "server started on port " <> tshow port
      settings = defaultSettings
        & setHost "*6"
        & setPort port
        & setBeforeMainLoop entry
      messages f = do
        red <- atomically $ dupTChan redraw
        forever $ do
          st <- readTVarIO chatState
          yield $ View "Chat" $ renderText $ renderComments f st
          atomically $ readTChan red
      app = asks pathInfo >>= \case
        -- chat messages
        [] -> page $ messages $ take 500
        ["user", usr] -> page $ messages $ take 50 . filter p where
          p c = user c == usr || display_user c == usr
        ["channel", chan] -> page $ videos auth chan
        -- actions
        ["loadfile"] -> wai $ routePost err $ runWai $ do
          url <- join $ asks requestBS
          liftIO $ loadfile mpv $ decodeUtf8 url
          pure $ responsePlainStatus ok200 []
        -- docs
        ["doc"] -> pure $ responseRedirect "/doc/all/index.html"
        "doc" : _ -> wai $ appStatic err installPath
        -- static
        _ -> wai $ appStatic err "public"
      update f = do
        modifyTVar' chatState $ reseek . f
        writeTChan redraw ()
      append cs = atomically $ update $ play . _Just %~
        (latest .~ time (NE.last cs)) .
        (comments %~ SB.append (toList cs))
      load vid = startTask taskLoad $ do
        atomically $ do
          update $ play ?~ def
          writeTVar seek True
        Tv.Video { channel = Tv.Channel { _id = cid } }
          <- Tv.getVideo auth vid
        emotes <- loadEmotes cid
        runConduit $
          Tv.getVideoComments auth vid
          .| C.mapE (format emotes highlights)
          .| C.mapM_ append
        atomically $ update $ play . _Just . done .~ True
      unload = startTask taskLoad $
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
  contT_ $ withTask_ $ forever $ do
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
