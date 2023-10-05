module MpvChat.Emote
  ( Emote (..),
    Emotes,
    EmoteScope (..),
    EmoteSource (..),
    loadEmotes,
    twitchEmoteUrl,
    thirdPartyEmote,
  )
where

import Data.Maybe (fromJust)
import Data.Text.IO (putStrLn)
import Network.HTTP.Types.Status (notFound404)
import Network.Request (statusErr)
import qualified Network.Twitch as Tv
import qualified Network.Twitch.Bttv as Bt
import qualified Network.Twitch.Ffz as Fz

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
