module Network.Twitch
  ( Auth (..),
    Channel (..),
    ChannelId,
    Clip (..),
    Comment (..),
    Emoticon (..),
    Fragment (..),
    Images (..),
    Message (..),
    Slug,
    User (..),
    UserId,
    Video (..),
    VideoId,
    emoteUrl,
    getChannelVideos,
    getClip,
    getUserByName,
    getVideo,
    getVideoComments,
    parseChannelId,
    parseSlug,
    parseVideoId,
    userChannel,
  )
where

import Data.Aeson
  ( FromJSON,
    parseJSON,
    withObject,
    (.:),
    (.:?),
  )
import Data.Conduit (ConduitT, yield)
import Data.String (IsString (fromString))
import GHC.TypeLits
  ( KnownSymbol,
    Symbol,
    symbolVal,
  )
import Network.HTTP.Types (Query)
import Network.Request (request)
import Network.Twitch.Channel
  ( Channel (Channel),
    ChannelId,
    parseChannelId,
    userChannel,
  )
import Network.Twitch.Clip
  ( Clip (Clip),
    Slug,
    parseSlug,
  )
import Network.Twitch.Comment (Comment (Comment))
import Network.Twitch.Emoticon (Emoticon (Emoticon))
import Network.Twitch.Fragment (Fragment (Fragment))
import Network.Twitch.Message (Message (Message))
import Network.Twitch.User
  ( User (User),
    UserId,
  )
import Network.Twitch.Video
  ( Images (Images),
    Video (Video),
    VideoId,
    parseVideoId,
  )

newtype Auth = Auth
  { clientId :: ByteString
  }
  deriving stock (Show)

-- TODO: DataKinds is overkill,
-- either infer key from object keys,
-- or don't use FromJSON and manually convert from Value
data Paged (n :: Symbol) a = Paged
  { items :: [a],
    _next :: Maybe Text
  }
  deriving stock (Show)

instance (KnownSymbol n, FromJSON a) => FromJSON (Paged n a) where
  parseJSON = withObject "Paged" $ \v ->
    Paged
      <$> v .: fromString (symbolVal @n Proxy)
      <*> v .:? "_next"

emoteUrl :: Text -> Text
emoteUrl i = "https://static-cdn.jtvnw.net/emoticons/v1/" <> i <> "/2.0"

rootUrl :: Text
rootUrl = "https://api.twitch.tv/v5"

userUrl :: Text
userUrl = rootUrl <> "/users"

channelUrl :: ChannelId -> Text
channelUrl cid = rootUrl <> "/channels/" <> tshow cid

videosUrl :: ChannelId -> Text
videosUrl cid = channelUrl cid <> "/videos"

videoUrl :: VideoId -> Text
videoUrl vid = rootUrl <> "/videos/" <> tshow vid

commentsUrl :: VideoId -> Text
commentsUrl vid = videoUrl vid <> "/comments"

clipUrl :: Slug -> Text
clipUrl s = rootUrl <> "/clips/" <> tshow s

query :: (FromJSON a, MonadIO m) => Auth -> Text -> Query -> m a
query auth url q = request url q [("Client-ID", clientId auth)]

getOffsetPaged ::
  forall n a m proxy i.
  (KnownSymbol n, FromJSON a, MonadIO m) =>
  proxy n ->
  Int ->
  Auth ->
  Text ->
  ConduitT i [a] m ()
getOffsetPaged _ limit auth url = fetch 0
  where
    fetch off = do
      Paged xs _ <-
        query @(Paged n a)
          auth
          url
          [ ("limit", Just $ encodeUtf8 $ tshow limit),
            ("offset", Just $ encodeUtf8 $ tshow off)
          ]
      yield xs
      when (length xs == limit) $ fetch $ off + limit

getCursorPaged ::
  forall n a m proxy i.
  (KnownSymbol n, FromJSON a, MonadIO m) =>
  proxy n ->
  Auth ->
  Text ->
  ConduitT i [a] m ()
getCursorPaged _ auth url = fetch ""
  where
    fetch cur = do
      Paged xs nxt <-
        query @(Paged n a)
          auth
          url
          [ ("cursor", Just $ encodeUtf8 cur)
          ]
      yield xs
      for_ nxt fetch

getUserByName :: MonadIO m => Auth -> Text -> m (Maybe User)
getUserByName auth name = do
  Paged us _ <-
    query @(Paged "users" User)
      auth
      userUrl
      [ ("login", Just $ encodeUtf8 name)
      ]
  pure $ headMay us

getVideo :: MonadIO m => Auth -> VideoId -> m Video
getVideo auth vid = query auth (videoUrl vid) []

getClip :: MonadIO m => Auth -> Slug -> m Clip
getClip auth slug = query auth (clipUrl slug) []

getChannelVideos ::
  MonadIO m => Auth -> ChannelId -> ConduitT i [Video] m ()
getChannelVideos auth = getOffsetPaged @"videos" Proxy 100 auth . videosUrl

getVideoComments ::
  MonadIO m => Auth -> VideoId -> ConduitT i [Comment] m ()
getVideoComments auth = getCursorPaged @"comments" Proxy auth . commentsUrl
