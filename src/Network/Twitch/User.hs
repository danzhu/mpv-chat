module Network.Twitch.User
  ( User (..),
    UserId,
  )
where

import Data.Aeson (FromJSON)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)

newtype UserId = UserId Int
  deriving newtype (Eq, Show, FromJSON, Hashable, Ord, FromField, ToField)

data User = User
  { _id :: UserId,
    name :: Text,
    display_name :: Text,
    bio :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)
