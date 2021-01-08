module Network.Twitch.Message
  ( Message (..),
  )
where

import Data.Aeson (FromJSON)
import Network.Twitch.Fragment (Fragment)

data Message = Message
  { fragments :: NonEmpty Fragment,
    user_color :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)
