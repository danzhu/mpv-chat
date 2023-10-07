{-# LANGUAGE TemplateHaskell #-}

module MpvChat.Data
  ( Video (..),
    View (..),
    User (..),
    Badge (..),
    Highlight (..),
    Comment (..),
    ChatState (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import MpvChat.Emote (EmoteSource)
import qualified Network.Twitch as Tv

data View = View
  { title :: Text,
    content :: LText,
    scroll :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

makeFieldLabelsNoPrefix ''View

data Video = Video
  { id :: Tv.VideoId,
    title :: Text,
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
