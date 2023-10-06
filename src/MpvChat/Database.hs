module MpvChat.Database
  ( loadVideo,
    loadChapter,
    loadEmote,
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
    query_,
  )
import Database.Sqlite.Adapter (JSONField (JSONField))
import MpvChat.Data
  ( Badge,
    Comment (Comment),
    Highlight (Highlight, NameOnly, NoHighlight),
    User (User),
    Video (Video),
  )
import qualified MpvChat.Data
import MpvChat.Emote
  ( EmoteSource (DatabaseSource, UrlSource),
    loadEmotes,
  )
import qualified Network.Twitch as Tv
import Optics.Iso (Iso, iso)

_Only :: Iso (Only a) (Only b) a b
_Only = iso fromOnly Only

loadVideo :: Connection -> Tv.VideoId -> Bool -> IO Video
loadVideo conn vid online = do
  [(title, createdAt, channelId)] <-
    query
      conn
      "SELECT title, created_at, channel_id FROM video WHERE id = ?"
      (Only vid)
  emotes <-
    if online
      then UrlSource <$> loadEmotes channelId
      else
        DatabaseSource . setFromList . map fromOnly
          <$> query_ conn "SELECT name FROM emote_third_party"
  pure $ Video {id = vid, title, createdAt, channelId, emotes}

loadEmote :: Connection -> Text -> Bool -> IO (Maybe ByteString)
loadEmote conn id thirdParty =
  headOf (each % _Only) <$> query conn q (Only id)
  where
    q =
      if thirdParty
        then "SELECT data FROM emote_third_party WHERE name = ?"
        else "SELECT data FROM emote WHERE id = ?"

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

loadComments :: Connection -> Tv.VideoId -> UTCTime -> IO [Comment]
loadComments conn vid time =
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
      \ORDER BY c.created_at DESC \
      \LIMIT 500"
      (vid, time)
  where
    mkComment
      ( createdAt,
        JSONField fragments,
        JSONField badges :: JSONField [Badge],
        userColor,
        uid,
        displayName,
        name,
        bio,
        highlight
        ) = do
        Comment
          { createdAt,
            commenter = User {id = uid, displayName, name, bio},
            fragments,
            userColor,
            highlight =
              maximum
                [ maybe NoHighlight (bool NameOnly Highlight) highlight,
                  bool NoHighlight NameOnly $
                    elemOf (each % #_id) "moderator" badges,
                  bool NoHighlight NameOnly $
                    elemOf (each % #_id) "partner" badges
                ]
          }
