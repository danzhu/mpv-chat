module Network.Twitch.Bttv
  ( Bttv(..)
  , Emote(..)
  , channelUrl
  , globalUrl
  ) where

import           Prelude                 hiding ( id )
import           Data.Aeson                     ( FromJSON )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

data Bttv = Bttv
  { urlTemplate :: T.Text
  , emotes :: [Emote]
  } deriving (Generic, Show)

data Emote = Emote
  { id :: T.Text
  , code :: T.Text
  } deriving (Generic, Show)

instance FromJSON Bttv
instance FromJSON Emote

globalUrl :: T.Text
globalUrl = "https://api.betterttv.net/2/emotes"

channelUrl :: T.Text -> T.Text
channelUrl c = "https://api.betterttv.net/2/channels/" <> c
