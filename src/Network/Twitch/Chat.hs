{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Twitch.Chat
  ( Config(..)
  , run
  ) where

import           Control.Concurrent.Task        ( startTask
                                                , withEmptyTask
                                                , withTask_
                                                )
import           Control.Monad.ContT            ( contT_ )
import qualified Data.SeekBuffer               as SB
import           Network.Mpv                    ( MpvError(MpvIpcError)
                                                , getProperty
                                                , newMpvClient
                                                , observeEvent
                                                , observeProperty
                                                , runMpv
                                                )
import qualified Network.Twitch.Bttv           as Bt
import qualified Network.Twitch.Ffz            as Fz
import qualified Network.Twitch.Twitch         as Tv
import           Network.Wai.Application.Static ( appStatic )
import           Network.Wai.IO                 ( eventData
                                                , responseEvents
                                                , responsePlainStatus
                                                )
import           Network.Wai.Middleware.StaticRoute
                                                ( routeAccept
                                                , routeMethod
                                                )
import           Network.Wai.Monad              ( runWai
                                                , wai
                                                )

import           Control.Monad                  ( forever
                                                , guard
                                                , (<=<)
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.RWS              ( RWS
                                                , evalRWS
                                                , ask
                                                , asks
                                                , tell
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Cont       ( ContT(ContT)
                                                , evalContT
                                                )
import           Data.Bool                      ( bool )
import qualified Data.ByteString.Lazy          as LB
import           Data.Conduit                   ( runConduit
                                                , yield
                                                , (.|)
                                                )
import qualified Data.Conduit.Combinators      as C
import           Data.Default.Class             ( Default
                                                , def
                                                )
import           Data.Foldable                  ( fold
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( (&) )
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( isJust )
import           Data.Scientific                ( Scientific )
import qualified Data.Text                     as T
import           Lens.Micro                     ( _Just
                                                , (%~)
                                                , (.~)
                                                , (?~)
                                                , (^.)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Lucid.Base                     ( Html
                                                , HtmlT
                                                , renderBS
                                                , renderBST
                                                , toHtml
                                                , toHtmlRaw
                                                )
import           Lucid.Html5                    ( a_
                                                , class_
                                                , div_
                                                , href_
                                                , img_
                                                , li_
                                                , pre_
                                                , span_
                                                , src_
                                                , style_
                                                , title_
                                                , ul_
                                                )
import           Network.HTTP.Types.Header      ( hContentType
                                                , hLocation
                                                )
import           Network.HTTP.Types.Method      ( methodGet
                                                , methodHead
                                                )
import           Network.HTTP.Types.Status      ( ok200
                                                , temporaryRedirect307
                                                )
import           Network.URI                    ( parseURI
                                                , uriAuthority
                                                )
import           Network.Wai                    ( pathInfo
                                                , responseFile
                                                )
import           Network.Wai.Handler.Warp       ( Port
                                                , defaultSettings
                                                , runSettings
                                                , setBeforeMainLoop
                                                , setHost
                                                , setPort
                                                )
import           System.FilePath.Posix          ( takeDirectory )
import           Text.Printf                    ( printf )
import           UnliftIO.Async                 ( concurrently_
                                                , mapConcurrently
                                                )
import           UnliftIO.Environment           ( getExecutablePath )
import           UnliftIO.Exception             ( tryJust )
import           UnliftIO.STM                   ( atomically
                                                , dupTChan
                                                , modifyTVar'
                                                , newBroadcastTChanIO
                                                , newTVarIO
                                                , readTChan
                                                , readTVar
                                                , readTVarIO
                                                , registerDelay
                                                , writeTChan
                                                , writeTVar
                                                )

type Scope = T.Text
type Emote = (Scope, T.Text)
type Emotes = HM.HashMap T.Text Emote
type Mentions = [T.Text]
type Fmt = HtmlT (RWS Emotes Mentions ())

data Comment = Comment
  { time :: Scientific
  , html :: LB.ByteString
  , user :: T.Text
  , display_user :: T.Text
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

data State = State
  { _play :: Maybe Play
  , _delay :: Scientific
  }

makeLenses ''State

instance Default State where
  def = State Nothing 0

data Config = Config
  { ipcPath :: FilePath
  , auth :: Tv.Auth
  , port :: Port
  }
  deriving stock Show

bttv :: Scope -> [Bt.Emote] -> Emotes
bttv s = HM.fromList . map emote where
  emote e = (Bt.code e, ("bttv " <> s, Bt.emoteUrl $ Bt.id e))

bttvGlobal :: Bt.Global -> Emotes
bttvGlobal = bttv "global"

bttvChannel :: Bt.Channel -> Emotes
bttvChannel = chan <> shared where
  chan = bttv "channel" . Bt.channelEmotes
  shared = bttv "channel shared" . Bt.sharedEmotes

ffz :: Fz.Channel -> Emotes
ffz = HM.fromList . map emote . (Fz.emoticons <=< HM.elems . Fz.sets) where
  emote e = (Fz.name e, ("ffz channel", Fz.urls e HM.! 2))

loadEmotes :: MonadIO m => Tv.Channel -> m Emotes
loadEmotes chan = liftIO $ fold <$> mapConcurrently id
  [ bttvGlobal <$> Bt.getGlobal
  , bttvChannel <$> Bt.getChannel chan
  , ffz <$> Fz.getChannel chan
  ]

fmtComment :: Tv.Comment -> Fmt ()
fmtComment Tv.Comment { message = msg, commenter = usr } =
  li_ [class_ "comment"] $ do
    a_
      [ class_ "icon"
      , style_ $ maybe "" ("background-color: " <>) $ Tv.user_color msg
      , href_ $ "/user/" <> Tv.name usr
      ] $ fmtUser usr
    div_ [class_ "message"] $
      traverse_ fmtFragment $ Tv.fragments msg

fmtUser :: Tv.Commenter -> Fmt ()
fmtUser usr = div_ [class_ "details"] $ do
  span_ [class_ "name"] $ toHtml $ Tv.display_name usr
  for_ (Tv.bio usr) $ \b -> div_ $ do
    "Bio: "
    -- FIXME: find out why there are bad chars in twitch response,
    -- which kill the output if not escaped to ascii
    span_ [class_ "bio"] $ toHtml $ show b

fmtFragment :: Tv.Fragment -> Fmt ()
fmtFragment Tv.Fragment { text = txt, emoticon = emo }
  | Just e <- emo = fmtEmote "twitch" txt $ Tv.emoteUrl $ Tv.emoticon_id e
  | otherwise = fold $ L.intersperse " " $ map fmtWord $ T.split (== ' ') txt

fmtWord :: T.Text -> Fmt ()
fmtWord wor = ask >>= \emotes -> if
  | Just (ori, url) <- HM.lookup wor emotes -> fmtEmote ori wor url
  | Just ('@', nam) <- T.uncons wor -> do
      tell [nam]
      a_ [class_ "mention", href_ $ "/user/" <> nam] $ "@" <> toHtml nam
  | Just (uriAuthority -> Just _) <- parseURI $ T.unpack wor ->
      a_ [class_ "url", href_ wor] $ toHtml wor
  | otherwise -> toHtml wor

fmtEmote :: T.Text -> T.Text -> T.Text -> Fmt ()
fmtEmote ori txt url = img_
  [ class_ "emote"
  , src_ $ "https:" <> url
  , title_ $ txt <> " [" <> ori <> "]"
  ]

format :: Emotes -> Tv.Comment -> Comment
format e c = Comment
  { html = h
  , time = Tv.content_offset_seconds c
  , user = Tv.name u
  , display_user = Tv.display_name u
  , mentions = m
  }
  where
    u = Tv.commenter c
    (h, m) = evalRWS (renderBST $ fmtComment c) e ()

render :: ([Comment] -> [Comment]) -> State -> Html ()
render f st = case st ^. play of
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
      else ul_ [class_ "chat"] $ do
        let recent = f $ SB.past cms
        toHtmlRaw $ foldMap html recent

reseek :: State -> State
reseek st = play . _Just %~ upd $ st where
  upd p = comments %~ SB.seek adv $ p where
    adv c = time c < t
    t = p ^. current - st ^. delay

run :: Config -> IO ()
run conf = evalContT $ do
  state <- newTVarIO def
  active <- newTVarIO False
  seek <- newTVarIO False
  redraw <- newBroadcastTChanIO

  exePath <- getExecutablePath
  let installPath = takeDirectory $ takeDirectory exePath

  let entry = putStrLn $ "server started on port " <> show (port conf)
      settings = defaultSettings
        & setHost "*6"
        & setPort (port conf)
        & setBeforeMainLoop entry
      err stat _ res = res $ responsePlainStatus stat []
      pageHtml = pure $ responseFile ok200 hs "res/page.html" Nothing where
        hs = [(hContentType, "text/html")]
      pageEvents f = pure $ responseEvents $ do
        red <- atomically $ dupTChan redraw
        forever $ do
          st <- readTVarIO state
          yield def { eventData = renderBS $ f st }
          atomically $ readTChan red
      page f = wai p where
        p = routeMethod err
          [ (methodGet, get)
          -- TODO: manually check that no body is sent
          , (methodHead, get)
          ]
        get = routeAccept err
          [ ("text/html", runWai pageHtml)
          , ("text/event-stream", runWai $ pageEvents f)
          ]
      app = asks pathInfo >>= \case
        [] -> page $ render $ take 500
        ["user", usr] -> page $ render $ take 50 . filter p where
          p c = user c == usr || display_user c == usr
        ["doc"] -> pure $ responsePlainStatus temporaryRedirect307
          [(hLocation, "/doc/all/index.html")]
        "doc" : _ -> wai $ appStatic err installPath
        _ -> wai $ appStatic err "public"
  contT_ $ withTask_ $ runSettings settings $ runWai app

  mpv <- newMpvClient
  taskLoad <- ContT withEmptyTask
  let update f = do
        modifyTVar' state $ reseek . f
        writeTChan redraw ()
      append cs = atomically $ update $ play . _Just %~
        (latest .~ time (NE.last cs)) .
        (comments %~ SB.append (NE.toList cs))
      load vid = startTask taskLoad $ do
        atomically $ do
          update $ play ?~ def
          writeTVar seek True
        video <- Tv.getVideo (auth conf) vid
        emotes <- loadEmotes $ Tv.channel video
        runConduit $
          Tv.sourceComments (auth conf) vid
          .| C.mapE (format emotes)
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
  contT_ $ withTask_ $ forever $ do
    timeout <- registerDelay 1_000_000
    atomically $ do
      guard . isJust . (^. play) =<< readTVar state
      readTVar seek >>= \case
        True -> writeTVar seek False
        False -> do
          guard =<< readTVar active
          guard =<< readTVar timeout
    let unavail (MpvIpcError "property unavailable") = Just ()
        unavail _ = Nothing
        upd t = atomically $ update $ play . _Just . current .~ t
    either pure upd =<< tryJust unavail (getProperty mpv "playback-time")
  lift $ runMpv mpv (ipcPath conf) `concurrently_` setup
