module MpvChat.Chat
  ( renderComments,
  )
where

import Data.Fixed (div')
import Data.Time.Clock
  ( addUTCTime,
    nominalDiffTimeToSeconds,
  )
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple
  ( Connection,
    Only (Only),
    query,
  )
import Database.Sqlite.Adapter (JSONField (JSONField))
import Lucid.Base
  ( HtmlT,
    renderBST,
    toHtml,
    toHtmlRaw,
  )
import Lucid.Html5
  ( a_,
    alt_,
    class_,
    div_,
    href_,
    img_,
    li_,
    pre_,
    span_,
    src_,
    style_,
    title_,
    ul_,
  )
import MpvChat.Data
  ( Badge,
    ChatState (ChatState),
    Comment (Comment),
    Highlight (Highlight, NameOnly, NoHighlight),
    User (User),
    Video (Video),
  )
import qualified MpvChat.Data
import MpvChat.Emote
  ( Emote (Emote),
    EmoteScope (EmoteScope),
    EmoteSource,
    thirdPartyEmote,
    twitchEmoteUrl,
  )
import qualified Network.Twitch as Tv
import qualified Network.Twitch.Emoticon
import qualified Network.Twitch.Fragment
import Network.URI
  ( parseURI,
    uriAuthority,
  )

type Fmt = HtmlT (Reader EmoteSource)

fmtComment :: Comment -> Fmt ()
fmtComment
  Comment
    { fragments,
      commenter = commenter@User {id = commenterId, displayName},
      userColor,
      highlight
    } = do
    li_
      [ class_ $ bool "comment" "comment highlight" $ highlight == Highlight
      ]
      $ do
        a_
          [ class_ "icon",
            style_ $ maybe "" ("background-color: " <>) userColor,
            -- FIXME: user page removed
            href_ $ "/user/" <> tshow commenterId
          ]
          $ fmtUser commenter
        div_ [class_ "message"] do
          unless (highlight == NoHighlight) do
            span_
              [ class_ "name",
                style_ $ maybe "" ("color: " <>) userColor
              ]
              $ toHtml displayName
            ": "
          traverse_ fmtFragment fragments

fmtUser :: User -> Fmt ()
fmtUser User {displayName, name, bio} =
  div_ [class_ "details"] $ do
    span_ [class_ "name"] do
      toHtml displayName
      " ["
      toHtml name
      "]"
    for_ bio $ \b -> div_ $ do
      "Bio: "
      -- FIXME: find out why there are bad chars in twitch response,
      -- which kill the output if not escaped to ascii
      span_ [class_ "bio"] $ toHtml b

fmtFragment :: Tv.Fragment -> Fmt ()
fmtFragment Tv.Fragment {text, emoticon}
  | Just Tv.Emoticon {emoticon_id} <- emoticon = do
    emotes <- ask
    fmtEmote "twitch" text $ twitchEmoteUrl emotes emoticon_id
  | otherwise = fold $ intersperse " " $ fmtWord <$> splitElem ' ' text

fmtWord :: Text -> Fmt ()
fmtWord wor =
  ask >>= \emotes ->
    if
        | Just (Emote ori url) <- thirdPartyEmote emotes wor -> fmtEmote ori wor url
        | Just ('@', nam) <- uncons wor -> do
          -- FIXME: user page removed
          a_ [class_ "mention", href_ $ "/user/" <> nam] $ "@" <> toHtml nam
        | Just (uriAuthority -> Just _) <- parseURI $ toList wor ->
          a_ [class_ "url", href_ wor] $ toHtml wor
        | otherwise -> toHtml wor

fmtEmote :: EmoteScope -> Text -> Text -> Fmt ()
fmtEmote (EmoteScope ori) txt url =
  img_
    [ class_ "emote",
      src_ url,
      title_ $ txt <> " [" <> ori <> "]",
      alt_ txt
    ]

renderComments :: Connection -> ChatState -> HtmlT IO ()
renderComments
  conn
  ChatState
    { video = Just Video {id = vid, createdAt = startTime, emotes},
      playbackTime = Just playbackTime,
      delay
    } =
    do
      let subTime = playbackTime - delay
      let currentTime = addUTCTime subTime startTime
      chapters <-
        liftIO $
          query
            conn
            "SELECT description FROM chapter \
            \WHERE video_id = ? \
            \AND ? - start_milliseconds BETWEEN 0 AND length_milliseconds \
            \ORDER BY start_milliseconds \
            \LIMIT 1"
            (vid, nominalDiffTimeToSeconds subTime `div'` 0.001 :: Int)
      comments <-
        liftIO $
          query
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
            (vid, currentTime)
      pre_ $ do
        toHtml $ formatTime defaultTimeLocale "%F %T" currentTime
        " ["
        toHtml $ formatTime defaultTimeLocale "%h:%2M:%2S" subTime
        when (delay /= 0) do
          " "
          when (delay > 0) "+"
          toHtml $ tshow delay
        "] "
      for_ chapters \(Only desc) -> div_ $ toHtml (desc :: Text)
      ul_ [class_ "comments"] $
        for_
          comments
          \( createdAt,
             JSONField fragments,
             JSONField badges,
             userColor,
             uid,
             displayName,
             name,
             bio,
             highlight
             ) -> do
              let u = User {id = uid, displayName, name, bio}
                  hl = maybe NoHighlight (bool NameOnly Highlight) highlight
                  hlMod =
                    bool NoHighlight NameOnly $
                      elemOf (each % #_id) "moderator" (badges :: [Badge])
                  c =
                    Comment
                      { createdAt,
                        commenter = u,
                        fragments,
                        userColor,
                        highlight = max hl hlMod
                      }
                  html = runReader (renderBST $ fmtComment c) emotes
              toHtmlRaw html
renderComments _ _ = pre_ "idle"
