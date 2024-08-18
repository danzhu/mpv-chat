{-# LANGUAGE TemplateHaskell #-}

module Network.Twitch
  ( VideoId,
    UserId,
    ChannelId,
    Emoticon (..),
    Fragment (..),
    Badge (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)

newtype VideoId = VideoId Int64
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, FromField, ToField)

newtype UserId = UserId Int64
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, FromField, ToField)

newtype ChannelId = ChannelId Int64
  deriving newtype (Eq, Ord, Hashable, Show, Read, FromJSON, FromField, ToField)

newtype Emoticon = Emoticon
  { emoticon_id :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

makeFieldLabelsNoPrefix ''Emoticon

data Fragment = Fragment
  { text :: Text,
    emoticon :: Maybe Emoticon
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

makeFieldLabelsNoPrefix ''Fragment

data Badge = Badge
  { _id :: Text,
    version :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

makeFieldLabelsNoPrefix ''Badge
