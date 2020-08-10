module Network.Twitch.Ffz
  ( Channel(..)
  , Emote(..)
  , Set(..)
  , channelUrl
  , getChannel
  ) where

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           MpvChat.Prelude
import           Network.Request                ( request )
import qualified Network.Twitch                as Tv

data Emote = Emote
  { name :: T.Text
  , urls :: HM.HashMap Int T.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

newtype Set = Set
  { emoticons :: [Emote]
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

newtype Channel = Channel
  { sets :: HM.HashMap T.Text Set
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

rootUrl :: T.Text
rootUrl = "https://api.frankerfacez.com/v1"

channelUrl :: Tv.ChannelId -> T.Text
channelUrl c = rootUrl <> "/room/id/" <> T.pack (show c)

getChannel :: MonadIO m => Tv.ChannelId -> m Channel
getChannel c = request (channelUrl c) [] []
