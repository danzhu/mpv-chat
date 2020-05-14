{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import           Control.Concurrent.Task        ( killTask
                                                , runTask
                                                , withTask
                                                )
import           Network.Mpv                    ( observeEvent
                                                , observeProperty
                                                , tryGetProperty
                                                , withMpv
                                                )
import qualified Network.Twitch                as Tv
import qualified Network.Twitch.Bttv           as Bt
import qualified Network.Twitch.Ffz            as Fz
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
                                                , when
                                                , guard
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.STM              ( atomically )
import           Control.Monad.Trans.Cont       ( runContT
                                                , ContT(ContT)
                                                )
import           Control.Applicative            ( (<**>) )
import           Data.Conduit                   ( runConduit
                                                , (.|)
                                                , yield
                                                )
import qualified Data.Conduit.Combinators      as C
import           Data.Default                   ( Default
                                                , def
                                                )
import           Data.Foldable                  ( traverse_
                                                , fold
                                                )
import           Data.Function                  ( (&) )
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L
import           Data.Scientific                ( Scientific )
import qualified Data.Text                     as T
import           Data.Bool                      ( bool )
import           Lens.Micro                     ( each
                                                , has
                                                , (%~)
                                                , (.~)
                                                , (?~)
                                                , (^.)
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
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtml )
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Options.Applicative            ( Parser
                                                , auto
                                                , execParser
                                                , fullDesc
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , metavar
                                                , option
                                                , progDesc
                                                , short
                                                , showDefault
                                                , str
                                                , value
                                                )

type Scope = T.Text
type Emote = (Scope, T.Text)
type Emotes = HM.HashMap T.Text Emote

data Play = Play
  { _comments :: [(Scientific, H.Html)]
  , _done :: Bool
  , _time :: Scientific
  }

newtype State = State
  { _play :: Maybe Play
  }

data Config = Config
  { _ipcPath :: FilePath
  , _auth :: Tv.Auth
  , _port :: Port
  } deriving (Show)

makeLenses ''Play
makeLenses ''State
makeLenses ''Config

instance Default Play where
  def = Play
    { _comments = []
    , _done = False
    , _time = 0
    }

instance Default State where
  def = State { _play = Nothing }

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
  fragment frag = do
    let txt = Tv.text frag
    case Tv.emoticon frag of
      Nothing -> fold $ L.intersperse " " $ map word $ T.split (== ' ') txt
      Just e -> emote "twitch" txt $ Tv.emoteUrl $ Tv.emoticon_id e
  word wor
    | Just (ori, url) <- HM.lookup wor emotes = emote ori wor url
    | Just ('@', nam) <- T.uncons wor =
        H.span ! A.class_ "mention" $ "@" <> H.toMarkup nam
    | otherwise = H.toMarkup wor
  emote ori txt url = H.img ! A.class_ "emote"
    ! A.src (H.toValue $ "https:" <> url)
    ! A.title (H.toValue $ txt <> " [" <> ori <> "]")

render :: State -> H.Markup
render state = case state ^. play of
  Nothing -> H.pre "idle"
  Just pla -> case pla ^. comments of
    [] -> H.pre "loading..."
    latest : _ -> do
      let tim = pla ^. time
          don = pla ^. done
          later c = fst c > tim
      H.pre $ do
        H.toMarkup (round tim :: Int)
        " / "
        H.toMarkup (round $ fst latest :: Int)
        bool " +" "" don
      when (don || later latest) $
        H.ul ! A.class_ "chat" $
          foldMap snd $ take 100 $ dropWhile later $ pla ^. comments

parseAuth :: Parser Tv.Auth
parseAuth = Tv.Auth <$> cid where
  cid = option str $
    long "client-id" <>
    metavar "ID" <>
    help "twitch client id"

parseConfig :: Parser Config
parseConfig = Config <$> ipc <*> parseAuth <*> por where
  ipc = option str $
    long "ipc-path" <>
    metavar "PATH" <>
    help "mpv ipc path"
  por = option auto $
    short 'p' <>
    long "port" <>
    metavar "PORT" <>
    value 8192 <>
    help "server port" <>
    showDefault

main :: IO ()
main = do
  let inf = info (parseConfig <**> helper) $
        fullDesc <>
        progDesc "mpv chat"
  conf <- execParser inf
  state <- newTVarIO def
  active <- newTVarIO False
  seek <- newTVarIO False
  redraw <- newBroadcastTChanIO
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
        _ -> appStatic req res
  flip runContT pure $ do
    mpv <- ContT $ withMpv $ conf ^. ipcPath
    taskLoad <- ContT withTask
    asyncSync <- ContT $ withAsync $ forever $ do
      timeout <- registerDelay 1_000_000
      atomically $ do
        guard . has (play . each) =<< readTVar state
        readTVar seek >>= \case
          True -> writeTVar seek False
          False -> do
            guard =<< readTVar active
            guard =<< readTVar timeout
      tryGetProperty mpv "playback-time" >>= \case
        Left "property unavailable" -> pure ()
        Left e -> fail $ T.unpack e
        Right t -> atomically $ update $ play . each . time .~ t
    let forceSync = writeTVar seek True
        load vid = do
          atomically $ do
            update $ play ?~ def
            forceSync
          video <- Tv.getVideo (conf ^. auth) vid
          emotes <- loadEmotes $ Tv.channel video
          let fmt = Tv.content_offset_seconds &&& format emotes
              cat cs = do
                let cs' = reverse $ map fmt cs
                atomically $ update $ play . each . comments %~ (cs' <>)
          runConduit $ Tv.sourceComments (conf ^. auth) vid .| C.mapM_ cat
          atomically $ update $ play . each . done .~ True
    liftIO $ do
      link asyncSync
      observeProperty mpv "filename" $ \case
        Nothing -> do
          killTask taskLoad
          atomically $ update $ play .~ Nothing
        Just n -> runTask taskLoad $ load n
      observeProperty mpv "pause" $ atomically . writeTVar active . not
      observeEvent mpv "seek" $ atomically forceSync
      runSettings settings app
