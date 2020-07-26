module Network.Twitch.Ffz
  ( Channel(..)
  , Emote(..)
  , Set(..)
  , channelUrl
  , getChannel
  ) where

import           Network.Request                ( request )
import qualified Network.Twitch.Twitch         as Tv

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( FromJSON )
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           GHC.Generics                   ( Generic )

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

channelUrl :: Tv.Channel -> T.Text
channelUrl c = rootUrl <> "/room/id/" <> T.pack (show $ Tv._id c)

getChannel :: MonadIO m => Tv.Channel -> m Channel
getChannel c = request (channelUrl c) [] []
