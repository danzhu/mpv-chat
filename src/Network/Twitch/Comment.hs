module Network.Twitch.Comment
  ( Comment (..),
  )
where

import Data.Aeson (FromJSON)
import Network.Twitch.Message (Message)
import Network.Twitch.User (User)

data Comment = Comment
  { message :: Message,
    commenter :: User,
    content_offset_seconds :: Scientific
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)
