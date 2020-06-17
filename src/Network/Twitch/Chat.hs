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
import           Network.Wai.Monad              ( application
                                                , runApp
                                                )

import           Control.Arrow                  ( (&&&) )
import           Control.Monad                  ( forever
                                                , guard
                                                , unless
                                                , (<=<)
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Cont       ( ContT(ContT)
                                                , evalContT
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader     ( asks )
import           Data.Conduit                   ( runConduit
                                                , yield
                                                , (.|)
                                                )
import qualified Data.Conduit.Combinators      as C
import           Data.Default.Class             ( Default
                                                , def
                                                )
import           Data.Foldable                  ( fold
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
import           Network.URI                    ( parseURI
                                                , uriAuthority
                                                )
import           Network.Wai                    ( pathInfo )
import           Network.Wai.Handler.Warp       ( Port
                                                , defaultSettings
                                                , runSettings
                                                , setBeforeMainLoop
                                                , setHost
                                                , setPort
                                                )
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtml )
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Printf                    ( printf )
import           UnliftIO.Async                 ( concurrently_
                                                , mapConcurrently
                                                )
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

data Play = Play
  { _comments :: SB.SeekBuffer (Scientific, H.Html)
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
  deriving (Show)

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

format :: Emotes -> Tv.Comment -> H.Markup
format emotes = comment where
  comment com = H.li ! A.class_ "comment" $ do
    let msg = Tv.message com
        s = maybe "" ("background-color: " <>) $ Tv.user_color msg
    H.div ! A.class_ "icon detailed" ! A.style (H.toValue s) $
      H.div ! A.class_ "details" $ do
        let user = Tv.commenter com
        H.span $ H.toMarkup $ Tv.display_name user
        -- FIXME: completely breaks blaze
        -- maybe "" (H.div . H.preEscapedToMarkup) $ Tv.bio user
    H.div ! A.class_ "message" $
      traverse_ fragment $ Tv.fragments msg
  fragment Tv.Fragment { text = txt, emoticon = emo }
    | Just e <- emo = emote "twitch" txt $ Tv.emoteUrl $ Tv.emoticon_id e
    | otherwise = fold $ L.intersperse " " $ map word $ T.split (== ' ') txt
  word wor
    | Just (ori, url) <- HM.lookup wor emotes = emote ori wor url
    | Just ('@', nam) <- T.uncons wor =
        -- TODO: link/show mention
        H.span ! A.class_ "mention" $ "@" <> H.toMarkup nam
    | Just (uriAuthority -> Just _) <- parseURI $ T.unpack wor =
        H.a ! A.href (H.toValue wor) $ H.toMarkup wor
    | otherwise = H.toMarkup wor
  emote ori txt url = H.img ! A.class_ "emote"
    ! A.src (H.toValue $ "https:" <> url)
    ! A.title (H.toValue $ txt <> " [" <> ori <> "]")

render :: State -> H.Markup
render st = case st ^. play of
  Nothing -> H.pre "idle"
  Just (Play cms don cur lat) -> do
    H.pre $ do
      H.toMarkup (round cur :: Int)
      H.string $ printf " [%+d]" (round $ st ^. delay :: Int)
      " / "
      H.toMarkup (round lat :: Int)
      unless don " ..."
    if not don && null (SB.future cms)
      then H.pre "buffering..."
      else H.ul ! A.class_ "chat" $ foldMap snd $ take 500 $ SB.past cms

reseek :: State -> State
reseek st = play . _Just %~ upd $ st where
  upd p = comments %~ SB.seek adv $ p where
    adv (t', _) = t' < t
    t = p ^. current - st ^. delay

run :: Config -> IO ()
run conf = evalContT $ do
  state <- newTVarIO def
  active <- newTVarIO False
  seek <- newTVarIO False
  redraw <- newBroadcastTChanIO

  let entry = putStrLn $ "server started on port " <> show (port conf)
      settings = defaultSettings
        & setHost "*6"
        & setPort (port conf)
        & setBeforeMainLoop entry
      err stat = pure $ responsePlainStatus stat []
      events = pure $ responseEvents $ do
        red <- atomically $ dupTChan redraw
        forever $ do
          st <- readTVarIO state
          yield def { eventData = renderHtml $ render st }
          atomically $ readTChan red
      static = application $ appStatic (runApp . err) "public"
      app = asks pathInfo >>= \case
        ["events"] -> events
        _ -> static
  contT_ $ withTask_ $ runSettings settings $ runApp app

  mpv <- newMpvClient
  taskLoad <- ContT withEmptyTask
  let update f = do
        modifyTVar' state $ reseek . f
        writeTChan redraw ()
      load vid = startTask taskLoad $ do
        atomically $ do
          update $ play ?~ def
          writeTVar seek True
        video <- Tv.getVideo (auth conf) vid
        emotes <- loadEmotes $ Tv.channel video
        let fmt = Tv.content_offset_seconds &&& format emotes
            upd cs =
              (latest .~ fst (NE.last cs)) .
              (comments %~ SB.append (NE.toList cs))
            cat cs = atomically $ update $ play . _Just %~ upd (fmt <$> cs)
        runConduit $ Tv.sourceComments (auth conf) vid .| C.mapM_ cat
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
