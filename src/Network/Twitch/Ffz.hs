module Network.Twitch.Ffz
  ( Channel (..),
    Emote (..),
    Set (..),
    channelUrl,
    getChannel,
  )
where

import Data.Aeson (FromJSON)
import Network.Request (request)
import qualified Network.Twitch as Tv
import Prelude hiding (Set)

data Emote = Emote
  { name :: Text,
    urls :: HashMap Int Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

newtype Set = Set
  { emoticons :: [Emote]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

newtype Channel = Channel
  { sets :: HashMap Text Set
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

rootUrl :: Text
rootUrl = "https://api.frankerfacez.com/v1"

channelUrl :: Tv.ChannelId -> Text
channelUrl c = rootUrl <> "/room/id/" <> tshow c

getChannel :: MonadIO m => Tv.ChannelId -> m Channel
getChannel c = request (channelUrl c) [] []
