module Network.Twitch.User
  ( User (..),
    UserId,
  )
where

newtype UserId = UserId Text
  deriving newtype (Eq, FromJSON, Hashable, Ord)

instance Show UserId where
  show (UserId i) = toList i

data User = User
  { _id :: UserId,
    name :: Text,
    display_name :: Text,
    bio :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)
