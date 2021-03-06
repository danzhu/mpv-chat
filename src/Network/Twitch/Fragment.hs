module Network.Twitch.Fragment
  ( Fragment (..),
  )
where

import Data.Aeson (FromJSON)
import Network.Twitch.Emoticon (Emoticon)

data Fragment = Fragment
  { text :: Text,
    emoticon :: Maybe Emoticon
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)
