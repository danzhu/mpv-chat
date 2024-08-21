module MpvChat.Database
  ( loadVideo,
    loadVideos,
    loadChapter,
    loadEmote,
    loadFile,
    loadComments,
  )
where

import Data.Fixed (div')
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    nominalDiffTimeToSeconds,
  )
import Database.SQLite.Simple
  ( Connection,
    Only (Only, fromOnly),
    query,
  )
import Database.Sqlite.Adapter (JSONField (JSONField))
import MpvChat.Data
  ( Badge (Badge),
    Comment (Comment),
    Highlight (Highlight, NameOnly, NoHighlight),
    User (User),
    Video (Video),
    VideoContext (VideoContext),
  )
import qualified MpvChat.Data
import qualified Network.Twitch as Tv
import Optics.Iso (Iso, iso)

_Only :: Iso (Only a) (Only b) a b
_Only = iso fromOnly Only

loadVideo :: Connection -> Tv.VideoId -> IO Video
loadVideo conn vid = do
  [(title, createdAt, channelId)] <-
    query
      conn
      "SELECT title, created_at, channel_id FROM video WHERE id = ?"
      (Only vid)
  emotes <-
    mapFromList
      <$> query conn "SELECT name, data FROM emote_third_party WHERE video_id = ?" (Only vid)
  badges <-
    mapFromList . map mkBadge
      <$> query
        conn
        "SELECT name, version, title, description, bytes \
        \FROM twitch_badge \
        \WHERE video_id = ?"
        (Only vid)
  let context = VideoContext {emotes, badges}
  pure $ Video {id = vid, title, createdAt, channelId, context}
  where
    mkBadge (name, version, title, description, bytes) =
      (Tv.Badge name version, Badge {title, description, bytes})

-- TODO: make a separate Video type for listing
loadVideos :: Connection -> IO [Video]
loadVideos conn =
  map mkVideo
    <$> query
      conn
      "SELECT id, title, created_at, channel_id FROM video ORDER BY created_at DESC"
      ()
  where
    mkVideo (id, title, createdAt, channelId) =
      Video {id, title, createdAt, channelId, context = def}

loadEmote :: Connection -> Text -> IO (Maybe ByteString)
loadEmote conn id =
  headOf (each % _Only)
    <$> query
      conn
      "SELECT f.data FROM emote e \
      \JOIN file f ON f.id = e.data \
      \WHERE e.id = ?"
      (Only id)

loadFile :: Connection -> Text -> IO (Maybe ByteString)
loadFile conn id = headOf (each % _Only) <$> query conn "SELECT data FROM file WHERE id = ?" (Only id)

loadChapter :: Connection -> Tv.VideoId -> NominalDiffTime -> IO (Maybe Text)
loadChapter conn vid subTime =
  headOf (each % _Only)
    <$> query
      conn
      "SELECT description FROM chapter \
      \WHERE video_id = ? \
      \AND ? - start_milliseconds BETWEEN 0 AND length_milliseconds \
      \ORDER BY start_milliseconds \
      \LIMIT 1"
      (vid, nominalDiffTimeToSeconds subTime `div'` 0.001 :: Int)

loadComments :: Connection -> Tv.VideoId -> UTCTime -> Maybe Tv.UserId -> IO [Comment]
loadComments conn vid time uid =
  map mkComment
    <$> query
      conn
      "SELECT \
      \    c.created_at, c.fragments, c.user_badges, c.user_color, \
      \    u.id, u.display_name, u.name, u.bio, \
      \    f.highlight \
      \FROM comment c \
      \JOIN user u ON u.id = c.commenter \
      \LEFT JOIN follow f ON f.id = u.id \
      \WHERE c.content_id = ? AND c.created_at < ? \
      \    AND ifnull(c.commenter = ?, true) \
      \ORDER BY c.created_at DESC \
      \LIMIT 500"
      (vid, time, uid)
  where
    mkComment
      ( createdAt,
        JSONField fragments,
        JSONField userBadges,
        userColor,
        commenterId,
        displayName,
        name,
        bio,
        highlight
        ) = do
        Comment
          { createdAt,
            commenter = User {id = commenterId, displayName, name, bio},
            fragments,
            userBadges,
            userColor,
            highlight =
              maximum
                [ maybe NoHighlight (bool NameOnly Highlight) highlight,
                  bool NoHighlight NameOnly $
                    elemOf (each % #_id) "moderator" userBadges,
                  bool NoHighlight NameOnly $
                    elemOf (each % #_id) "partner" userBadges
                ]
          }
