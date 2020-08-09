module Network.Twitch.Bttv
  ( Channel(..)
  , Emote(..)
  , Global
  , channelUrl
  , emoteUrl
  , getChannel
  , getGlobal
  , globalUrl
  ) where

import           Network.Request                ( request )
import qualified Network.Twitch.Twitch         as Tv

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( FromJSON )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Prelude                 hiding ( id )

-- HACK: v3 shared emote has different structure than global/channel,
-- here are common fields to avoid different types
data Emote = Emote
  { id :: T.Text
  , code :: T.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

type Global = [Emote]

data Channel = Channel
  { channelEmotes :: [Emote]
  , sharedEmotes :: [Emote]
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

rootUrl :: T.Text
rootUrl = "https://api.betterttv.net/3"

globalUrl :: T.Text
globalUrl = rootUrl <> "/cached/emotes/global"

channelUrl :: Tv.ChannelId -> T.Text
channelUrl c = rootUrl <> "/cached/users/twitch/" <> T.pack (show c)

emoteUrl :: T.Text -> T.Text
emoteUrl i = "//cdn.betterttv.net/emote/" <> i <> "/2x"

getGlobal :: MonadIO m => m Global
getGlobal = request globalUrl [] []

getChannel :: MonadIO m => Tv.ChannelId -> m Channel
getChannel c = request (channelUrl c) [] []
