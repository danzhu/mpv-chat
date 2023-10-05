{-# LANGUAGE TemplateHaskell #-}

module MpvChat.Data
  ( Video (..),
    User (..),
    Badge (..),
    Highlight (..),
    Comment (..),
    ChatState (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import MpvChat.Emote (EmoteSource)
import qualified Network.Twitch as Tv

data Video = Video
  { id :: Tv.VideoId,
    createdAt :: UTCTime,
    channelId :: Tv.ChannelId,
    emotes :: EmoteSource
  }

makeFieldLabelsNoPrefix ''Video

data User = User
  { id :: Tv.UserId,
    displayName :: Text,
    name :: Text,
    bio :: Maybe Text
  }

makeFieldLabelsNoPrefix ''User

data Badge = Badge
  { _id :: Text,
    version :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

makeFieldLabelsNoPrefix ''Badge

data Highlight
  = NoHighlight
  | NameOnly
  | Highlight
  deriving stock (Eq, Ord)

data Comment = Comment
  { createdAt :: UTCTime,
    commenter :: User,
    fragments :: [Tv.Fragment],
    userColor :: Maybe Text,
    highlight :: Highlight
  }

makeFieldLabelsNoPrefix ''Comment

data ChatState = ChatState
  { video :: Maybe Video,
    playbackTime :: Maybe NominalDiffTime,
    delay :: NominalDiffTime,
    version :: Int
  }

makeFieldLabelsNoPrefix ''ChatState

instance Default ChatState where
  def =
    ChatState
      { video = Nothing,
        playbackTime = Nothing,
        delay = 0,
        version = 0
      }
