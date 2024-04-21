{-# LANGUAGE TemplateHaskell #-}

module MpvChat.Data
  ( View (..),
    EmoteScope (..),
    EmoteSource,
    Video (..),
    User (..),
    Highlight (..),
    Comment (..),
    ChatState (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Network.Twitch as Tv

data View = View
  { title :: Text,
    content :: LText,
    scroll :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

makeFieldLabelsNoPrefix ''View

data EmoteScope
  = EmoteTwitch
  | EmoteThirdParty

-- third party emote text -> emote hash
type EmoteSource = HashMap Text Text

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
