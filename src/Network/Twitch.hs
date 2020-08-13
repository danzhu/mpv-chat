module Network.Twitch
  ( Auth(..)
  , Channel(..)
  , ChannelId
  , Clip(..)
  , Comment(..)
  , Commenter(..)
  , Emoticon(..)
  , Fragment(..)
  , Message(..)
  , Slug
  , Video(..)
  , VideoId
  , emoteUrl
  , getClip
  , getVideo
  , parseSlug
  , parseChannelId
  , parseVideoId
  , sourceComments
  ) where

import           Data.Char                      ( isDigit )
import           MpvChat.Prelude
import           Network.HTTP.Types             ( Query )
import           Network.Request                ( request )
import           Text.Megaparsec                ( Parsec
                                                , runParser
                                                , takeWhile1P
                                                , eof
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Text.Megaparsec.Error          ( errorBundlePretty )

type Parser = Parsec Void Text

newtype ChannelId = ChannelId Int
  deriving newtype (Eq, FromJSON, Hashable, Ord, Show)

newtype VideoId = VideoId Text
  deriving newtype (Eq, FromJSON, Hashable, Ord, Show)

-- | Clip id
newtype Slug = Slug Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, Hashable)

newtype Auth = Auth
  { clientId :: ByteString
  }
  deriving stock Show

newtype Channel = Channel
  { _id :: ChannelId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

newtype Video = Video
  { channel :: Channel
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

newtype Emoticon = Emoticon
  { emoticon_id :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

data Fragment = Fragment
  { text     :: Text
  , emoticon :: Maybe Emoticon
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

data Message = Message
  { fragments  :: NonEmpty Fragment
  , user_color :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

data Commenter = Commenter
  { name         :: Text
  , display_name :: Text
  , bio          :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

data Comment = Comment
  { message                :: Message
  , commenter              :: Commenter
  , content_offset_seconds :: Scientific
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

data Comments = Comments
  { comments :: NonEmpty Comment
  , _next    :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

data Thumbnails = Thumbnails
  { medium :: Text
  , small :: Text
  , tiny :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, Hashable)

data Clip = Clip
  { title :: Text
  , thumbnails :: Thumbnails
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, Hashable)

rootUrl :: Text
rootUrl = "https://api.twitch.tv/v5"

videoUrl :: VideoId -> Text
videoUrl (VideoId vid) = rootUrl <> "/videos/" <> vid

commentsUrl :: VideoId -> Text
commentsUrl vid = videoUrl vid <> "/comments"

emoteUrl :: Text -> Text
emoteUrl i = "//static-cdn.jtvnw.net/emoticons/v1/" <> i <> "/2.0"

clipUrl :: Slug -> Text
clipUrl (Slug s) = rootUrl <> "/clips/" <> s

query :: (MonadIO m, FromJSON a) => Auth -> Text -> Query -> m a
query auth url q = request url q [("Client-ID", clientId auth)]

parseChannelId :: Text -> Either String ChannelId
parseChannelId = first errorBundlePretty . runParser (p <* eof) "" where
  p :: Parser ChannelId
  p = ChannelId <$> decimal

parseVideoId :: Text -> Either String VideoId
parseVideoId = first errorBundlePretty . runParser (p <* eof) "" where
  p :: Parser VideoId
  p = VideoId <$> takeWhile1P (Just "video id") isDigit

parseSlug :: Text -> Either String Slug
parseSlug = pure . Slug

getVideo :: MonadIO m => Auth -> VideoId -> m Video
getVideo auth vid = query auth (videoUrl vid) []

getClip :: MonadIO m => Auth -> Slug -> m Clip
getClip auth slug = query auth (clipUrl slug) []

sourceComments :: MonadIO m => Auth -> VideoId -> ConduitT i (NonEmpty Comment) m ()
sourceComments auth vid = fetch "" where
  fetch cur = do
    cs <- query auth (commentsUrl vid) [("cursor", Just $ encodeUtf8 cur)]
    yield $ comments cs
    traverse_ fetch $ _next cs
