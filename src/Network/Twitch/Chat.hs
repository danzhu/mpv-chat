{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Twitch.Chat
  ( Config(..)
  , run
  ) where

import           Control.Concurrent.Task        ( killTask
                                                , runTask
                                                , withTask
                                                )
import qualified Data.SeekBuffer               as SB
import           Network.Mpv                    ( observeEvent
                                                , observeProperty
                                                , tryGetProperty
                                                , withMpv
                                                )
import qualified Network.Twitch.Bttv           as Bt
import qualified Network.Twitch.Ffz            as Fz
import qualified Network.Twitch.Twitch         as Tv
import           Network.Wai.Application.Static ( appStatic )
import           Network.Wai.Response           ( responseEvents )

import           Control.Arrow                  ( (&&&) )
import           Control.Concurrent.Async       ( link
                                                , mapConcurrently
                                                , withAsync
                                                )
import           Control.Concurrent.STM.TChan   ( dupTChan
                                                , newBroadcastTChanIO
                                                , readTChan
                                                , writeTChan
                                                )
import           Control.Concurrent.STM.TVar    ( modifyTVar'
                                                , newTVarIO
                                                , readTVar
                                                , readTVarIO
                                                , registerDelay
                                                , writeTVar
                                                )
import           Control.Monad                  ( forever
                                                , guard
                                                , unless
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.STM              ( atomically )
import           Control.Monad.Trans.Cont       ( ContT(ContT)
                                                , runContT
                                                )
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
import           Data.Scientific                ( Scientific )
import qualified Data.Text                     as T
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Lens.Micro                     ( _1
                                                , _Just
                                                , _last
                                                , (%~)
                                                , (.~)
                                                , (?~)
                                                , (^.)
                                                , (^?)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Network.Wai                    ( pathInfo )
import           Network.Wai.Handler.Warp       ( Port
                                                , defaultSettings
                                                , runSettings
                                                , setBeforeMainLoop
                                                , setHost
                                                , setPort
                                                )
import           Network.URI                    ( parseURI
                                                , uriAuthority
                                                )
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtml )
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

type Scope = T.Text
type Emote = (Scope, T.Text)
type Emotes = HM.HashMap T.Text Emote

data Play = Play
  { _comments :: SB.SeekBuffer Scientific H.Html
  , _done :: Bool
  , _latest :: Scientific
  }

makeLenses ''Play

instance Default Play where
  def = Play (SB.empty 0) False 0

type State = Maybe Play

data Config = Config
  { _ipcPath :: FilePath
  , _auth :: Tv.Auth
  , _port :: Port
  }
  deriving (Show)

makeLenses ''Config

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
ffz d = HM.fromList $ map emote . Fz.emoticons =<< HM.elems (Fz.sets d) where
  emote e = (Fz.name e, ("ffz channel", Fz.urls e HM.! 2))

loadEmotes :: Tv.Channel -> IO Emotes
loadEmotes chan = fold <$> mapConcurrently id
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
render = \case
  Nothing -> H.pre "idle"
  Just (Play cms don lat) -> do
    let tim = SB.current cms
    H.pre $ do
      H.toMarkup (round tim :: Int)
      " / "
      H.toMarkup (round lat :: Int)
      unless don " ..."
    if not don && tim > lat
      then H.pre "buffering..."
      else H.ul ! A.class_ "chat" $ foldMap snd $ take 100 $ SB.past cms

run :: Config -> IO ()
run conf = flip runContT pure $ do
  state <- liftIO $ newTVarIO Nothing
  active <- liftIO $ newTVarIO False
  seek <- liftIO $ newTVarIO False
  redraw <- liftIO newBroadcastTChanIO
  let entry = putStrLn $ "server started on port " <> show (conf ^. port)
      update f = do
        modifyTVar' state f
        writeTChan redraw ()
      events = do
        red <- liftIO $ atomically $ dupTChan redraw
        forever $ do
          s <- liftIO $ readTVarIO state
          yield $ renderHtml $ render s
          liftIO $ atomically $ readTChan red
      settings = defaultSettings
        & setHost "*6"
        & setPort (conf ^. port)
        & setBeforeMainLoop entry
      app req res = case pathInfo req of
        ["events"] -> res $ responseEvents events
        _ -> appStatic "public" req res
  mpv <- ContT $ withMpv $ conf ^. ipcPath
  taskLoad <- ContT withTask
  asyncSync <- ContT $ withAsync $ forever $ do
    timeout <- registerDelay 1_000_000
    atomically $ do
      guard . isJust =<< readTVar state
      readTVar seek >>= \case
        True -> writeTVar seek False
        False -> do
          guard =<< readTVar active
          guard =<< readTVar timeout
    tryGetProperty mpv "playback-time" >>= \case
      Left "property unavailable" -> pure ()
      Left e -> fail $ T.unpack e
      Right t -> atomically $ update $ _Just . comments %~ SB.seek t
  let forceSync = writeTVar seek True
      load vid = do
        atomically $ do
          update $ id ?~ def
          forceSync
        video <- Tv.getVideo (conf ^. auth) vid
        emotes <- loadEmotes $ Tv.channel video
        let fmt = Tv.content_offset_seconds &&& format emotes
            cat cs = atomically $ update $ _Just %~
              (latest %~ flip fromMaybe (cs' ^? _last . _1)) .
              (comments %~ SB.append cs')
              where cs' = map fmt cs
        runConduit $ Tv.sourceComments (conf ^. auth) vid .| C.mapM_ cat
        atomically $ update $ _Just . done .~ True
      unload = do
        killTask taskLoad
        atomically $ update $ id .~ Nothing
  liftIO $ link asyncSync
  liftIO $ observeProperty mpv "filename/no-ext" $ \case
    Nothing -> unload
    Just n -> case Tv.parseVideoId n of
      Left _ -> unload
      Right vid -> runTask taskLoad $ load vid
  liftIO $ observeProperty mpv "pause" $ atomically . writeTVar active . not
  liftIO $ observeEvent mpv "seek" $ atomically forceSync
  liftIO $ runSettings settings app
