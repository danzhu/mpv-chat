module Network.Twitch.Message
  ( Message (..),
  )
where

import Data.Aeson (FromJSON)
import Network.Twitch.Fragment (Fragment)

data Message = Message
  { body :: Text,
    fragments :: Maybe (NonEmpty Fragment),
    user_color :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)
