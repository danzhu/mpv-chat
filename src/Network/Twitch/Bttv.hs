module Network.Twitch.Bttv
  ( Channel(..)
  , Emote(..)
  , Global
  , channelUrl
  , emoteUrl
  , globalUrl
  ) where

import qualified Network.Twitch                as Tv

import           Prelude                 hiding ( id )
import           Data.Aeson                     ( FromJSON )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

-- HACK: v3 shared emote has different structure than global/channel,
-- here are common fields to avoid different types
data Emote = Emote
  { id :: T.Text
  , code :: T.Text
  } deriving (Generic, Show)

type Global = [Emote]

data Channel = Channel
  { channelEmotes :: [Emote]
  , sharedEmotes :: [Emote]
  } deriving (Generic, Show)

instance FromJSON Emote
instance FromJSON Channel

rootUrl :: T.Text
rootUrl = "https://api.betterttv.net/3"

globalUrl :: T.Text
globalUrl = rootUrl <> "/cached/emotes/global"

channelUrl :: Tv.Channel -> T.Text
channelUrl c = rootUrl <> "/cached/users/twitch/" <> T.pack (show $ Tv._id c)

emoteUrl :: T.Text -> T.Text
emoteUrl i = "//cdn.betterttv.net/emote/" <> i <> "/2x"
