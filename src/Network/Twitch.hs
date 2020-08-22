module Network.Twitch
  ( Auth(..)
  , Channel(..)
  , ChannelId
  , Clip(..)
  , Comment(..)
  , Emoticon(..)
  , Fragment(..)
  , Message(..)
  , Slug
  , User(..)
  , Video(..)
  , VideoId
  , emoteUrl
  , getClip
  , getVideo
  , parseChannelId
  , parseSlug
  , parseVideoId
  , sourceComments
  ) where

import           MpvChat.Prelude
import           Network.HTTP.Types             ( Query )
import           Network.Request                ( request )
import           Network.Twitch.Channel         ( Channel(Channel)
                                                , ChannelId
                                                , parseChannelId
                                                )
import           Network.Twitch.Clip            ( Slug
                                                , Clip(Clip)
                                                , parseSlug
                                                )
import           Network.Twitch.Comment         ( Comment(Comment) )
import           Network.Twitch.Emoticon        ( Emoticon(Emoticon) )
import           Network.Twitch.Fragment        ( Fragment(Fragment) )
import           Network.Twitch.Message         ( Message(Message) )
import           Network.Twitch.User            ( User(User) )
import           Network.Twitch.Video           ( Video(Video)
                                                , VideoId
                                                , parseVideoId
                                                )

newtype Auth = Auth
  { clientId :: ByteString
  }
  deriving stock Show

data Comments = Comments
  { comments :: NonEmpty Comment
  , _next    :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

rootUrl :: Text
rootUrl = "https://api.twitch.tv/v5"

videoUrl :: VideoId -> Text
videoUrl vid = rootUrl <> "/videos/" <> tshow vid

commentsUrl :: VideoId -> Text
commentsUrl vid = videoUrl vid <> "/comments"

emoteUrl :: Text -> Text
emoteUrl i = "//static-cdn.jtvnw.net/emoticons/v1/" <> i <> "/2.0"

clipUrl :: Slug -> Text
clipUrl s = rootUrl <> "/clips/" <> tshow s

query :: (MonadIO m, FromJSON a) => Auth -> Text -> Query -> m a
query auth url q = request url q [("Client-ID", clientId auth)]

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
