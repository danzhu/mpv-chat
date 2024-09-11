{-# LANGUAGE TemplateHaskell #-}

module MpvChat.Data
  ( View (..),
    EmoteScope (..),
    Badge (..),
    VideoContext (..),
    Video (..),
    User (..),
    Highlight (..),
    Comment (..),
    ChatState (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Database.SQLite.Simple (FromRow (fromRow), field)
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

data Badge = Badge
  { title :: Text,
    description :: Text,
    bytes :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromRow)

makeFieldLabelsNoPrefix ''Badge

data VideoContext = VideoContext
  { -- third party emote text -> emote hash
    emotes :: HashMap Text Text,
    badges :: HashMap Tv.Badge Badge
  }

makeFieldLabelsNoPrefix ''VideoContext

instance Default VideoContext where
  def = VideoContext mempty mempty

data Video = Video
  { id :: Tv.VideoId,
    title :: Text,
    createdAt :: UTCTime,
    channelId :: Tv.ChannelId,
    context :: VideoContext
  }

instance FromRow Video where
  fromRow = Video <$> field <*> field <*> field <*> field <*> pure def

makeFieldLabelsNoPrefix ''Video

data User = User
  { id :: Tv.UserId,
    displayName :: Text,
    name :: Text,
    bio :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromRow)

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
    userBadges :: [Tv.Badge],
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
