module Network.Twitch.Comment
  ( Comment (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Time.Clock (NominalDiffTime)
import Network.Twitch.Message (Message)
import Network.Twitch.User (User)

data Comment = Comment
  { message :: Message,
    commenter :: User,
    content_offset_seconds :: NominalDiffTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)
