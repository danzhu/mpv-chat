{-# LANGUAGE DataKinds #-}

module Network.Twitch
  ( Auth(..)
  , Channel(..)
  , ChannelId
  , Clip(..)
  , Comment(..)
  , Emoticon(..)
  , Fragment(..)
  , Images(..)
  , Message(..)
  , Slug
  , User(..)
  , Video(..)
  , VideoId
  , emoteUrl
  , getChannelVideos
  , getClip
  , getVideo
  , getVideoComments
  , parseChannelId
  , parseSlug
  , parseVideoId
  ) where

import           Data.Aeson                     ( parseJSON
                                                , withObject
                                                , (.:)
                                                , (.:?)
                                                )
import qualified Data.Conduit.Combinators      as C
import           GHC.TypeLits                   ( KnownSymbol
                                                , Symbol
                                                , symbolVal
                                                )
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
import           Network.Twitch.Video           ( Images(Images)
                                                , Video(Video)
                                                , VideoId
                                                , parseVideoId
                                                )

newtype Auth = Auth
  { clientId :: ByteString
  }
  deriving stock Show

-- FIXME: empty result is likely possible,
-- when offset = length and no results in paged
-- TODO: DataKinds is overkill,
-- either infer key from object keys,
-- or don't use FromJSON and manually convert from Value
data Paged (n :: Symbol) a = Paged
  { items :: NonEmpty a
  , _next :: Maybe Text
  }
  deriving stock Show

instance (KnownSymbol n, FromJSON a) => FromJSON (Paged n a) where
  parseJSON = withObject "Paged" $ \v -> Paged
    <$> v .: fromList (symbolVal @n Proxy)
    <*> v .:? "_next"

emoteUrl :: Text -> Text
emoteUrl i = "//static-cdn.jtvnw.net/emoticons/v1/" <> i <> "/2.0"

rootUrl :: Text
rootUrl = "https://api.twitch.tv/v5"

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
  proxy n -> Int -> Auth -> Text -> ConduitT i (NonEmpty a) m ()
getOffsetPaged _ limit auth url =
  C.yieldMany [0, limit ..]
  .| C.mapM fetch
  .| C.takeWhile full
  where
    fetch off = items <$> query @(Paged n a) auth url
      [ ("limit", Just $ encodeUtf8 $ tshow limit)
      , ("offset", Just $ encodeUtf8 $ tshow off)
      ]
    full xs = length xs == limit

getCursorPaged ::
  forall n a m proxy i.
  (KnownSymbol n, FromJSON a, MonadIO m) =>
  proxy n -> Auth -> Text -> ConduitT i (NonEmpty a) m ()
getCursorPaged _ auth url = fetch "" where
  fetch cur = do
    Paged xs nxt <- query @(Paged n a) auth url
      [ ("cursor", Just $ encodeUtf8 cur)
      ]
    yield xs
    for_ nxt fetch

getVideo :: MonadIO m => Auth -> VideoId -> m Video
getVideo auth vid = query auth (videoUrl vid) []

getClip :: MonadIO m => Auth -> Slug -> m Clip
getClip auth slug = query auth (clipUrl slug) []

getChannelVideos ::
  MonadIO m => Auth -> ChannelId -> ConduitT i (NonEmpty Video) m ()
getChannelVideos auth = getOffsetPaged @"videos" Proxy 100 auth . videosUrl

getVideoComments ::
  MonadIO m => Auth -> VideoId -> ConduitT i (NonEmpty Comment) m ()
getVideoComments auth = getCursorPaged @"comments" Proxy auth . commentsUrl
