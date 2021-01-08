module Network.Twitch.Clip
  ( Clip (..),
    Slug,
    Thumbnails (..),
    parseSlug,
  )
where

import MpvChat.Prelude

-- | Clip id.
newtype Slug = Slug Text
  deriving newtype (Eq, Ord, Show, FromJSON, Hashable)

data Thumbnails = Thumbnails
  { medium :: Text,
    small :: Text,
    tiny :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, Hashable)

data Clip = Clip
  { title :: Text,
    thumbnails :: Thumbnails
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, Hashable)

parseSlug :: Text -> Either String Slug
parseSlug = pure . Slug
