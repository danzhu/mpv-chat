module Network.Twitch.Ffz
  ( Channel(..)
  , Emote(..)
  , Set(..)
  , channelUrl
  , getChannel
  ) where

import           Network.Request                ( request )
import qualified Network.Twitch                as Tv

import           Data.Aeson                     ( FromJSON )
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           GHC.Generics                   ( Generic )

data Emote = Emote
  { name :: T.Text
  , urls :: HM.HashMap Int T.Text
  } deriving (Generic, Show)

newtype Set = Set
  { emoticons :: [Emote]
  } deriving (Generic, Show)

newtype Channel = Channel
  { sets :: HM.HashMap T.Text Set
  } deriving (Generic, Show)

instance FromJSON Channel
instance FromJSON Set
instance FromJSON Emote

rootUrl :: T.Text
rootUrl = "https://api.frankerfacez.com/v1"

channelUrl :: Tv.Channel -> T.Text
channelUrl c = rootUrl <> "/room/id/" <> T.pack (show $ Tv._id c)

getChannel :: Tv.Channel -> IO Channel
getChannel c = request (channelUrl c) [] []
