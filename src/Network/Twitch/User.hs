module Network.Twitch.User
  ( User(..)
  ) where

import           MpvChat.Prelude

data User = User
  { name :: Text
  , display_name :: Text
  , bio :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)
