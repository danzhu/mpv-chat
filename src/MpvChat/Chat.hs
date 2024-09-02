module MpvChat.Chat
  ( renderChat,
  )
where

import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple (Connection)
import Lucid.Base
  ( HtmlT,
    renderTextT,
    toHtml,
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
  ( Badge (Badge),
    Comment (Comment),
    EmoteScope (EmoteThirdParty, EmoteTwitch),
    Highlight (Highlight, NoHighlight),
    User (User),
    Video (Video),
    VideoContext (VideoContext),
    View (View),
  )
import qualified MpvChat.Data
import MpvChat.Database (loadChapter, loadComments)
import qualified Network.Twitch as Tv
import Network.URI
  ( parseURI,
    uriAuthority,
  )
import Optics.AffineFold (afolding)

type Fmt = HtmlT (RWS VideoContext () (HashMap Text Comment))

fmtComment :: Comment -> Fmt ()
fmtComment
  c@Comment
    { commenter = User {id = uid, displayName, name, bio},
      fragments,
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
            href_ $ "/user/" <> tshow uid
          ]
          $ div_ [class_ "details"]
          $ do
            div_ [class_ "display-name"] $ fmtUser c
            for_ bio $ \b -> div_ [class_ "bio"] $ toHtml b
        div_ [class_ "message"] do
          unless (highlight == NoHighlight) do
            fmtUser c
            ": "
          traverse_ fmtFragment fragments
    modify' $ insertMap displayName c . insertMap name c

fmtUser :: Comment -> Fmt ()
fmtUser
  Comment
    { commenter = User {displayName, name},
      userBadges,
      userColor
    } =
    span_
      [ class_ "name",
        style_ $ maybe "" ("color: " <>) userColor
      ]
      do
        VideoContext {badges} <- ask
        forOf_
          (each % afolding (`lookup` badges))
          userBadges
          \Badge {title, bytes} -> do
            img_
              [ class_ "badge",
                src_ $ "/file/" <> bytes,
                title_ title,
                alt_ title
              ]
            " "
        toHtml displayName
        when (name /= toLower displayName) do
          " ["
          toHtml name
          "]"

fmtFragment :: Tv.Fragment -> Fmt ()
fmtFragment Tv.Fragment {text, emoticon}
  | Just Tv.Emoticon {emoticon_id} <- emoticon =
      fmtEmote EmoteTwitch text $ "/emote/" <> emoticon_id
  | otherwise = fold $ intersperse " " $ fmtWord <$> splitElem ' ' text

fmtWord :: Text -> Fmt ()
fmtWord word = do
  VideoContext {emotes} <- ask
  if
    | Just id <- lookup word emotes ->
        fmtEmote EmoteThirdParty word $ "/file/" <> id
    | Just ('@', name) <- uncons word ->
        -- FIXME: mention doesn't work on user page,
        -- since the filtering happens in db
        span_ [class_ "mention"] do
          "@"
          -- TODO: create color index at video start,
          -- so that it works across long distances?
          gets (lookup name) >>= \case
            Nothing -> toHtml name
            Just Comment {userColor, commenter = User {id = uid}} ->
              a_
                [ class_ "name",
                  style_ $ maybe "" ("color: " <>) userColor,
                  href_ $ "/user/" <> tshow uid
                ]
                $ toHtml name
    | Just (uriAuthority -> Just _) <- parseURI $ toList word ->
        span_ [class_ "url"] $ toHtml word
    | otherwise -> toHtml word

fmtEmote :: EmoteScope -> Text -> Text -> Fmt ()
fmtEmote scope txt url =
  img_
    [ class_ "emote",
      src_ url,
      title_ $ txt <> " [" <> ori <> "]",
      alt_ txt
    ]
  where
    ori = case scope of
      EmoteTwitch -> "twitch"
      EmoteThirdParty -> "third-party"

renderChat ::
  Connection ->
  Video ->
  NominalDiffTime ->
  NominalDiffTime ->
  Maybe Tv.UserId ->
  IO View
renderChat
  conn
  Video {id = vid, title, createdAt = startTime, context}
  playbackTime
  delay
  uid = do
    let subTime = playbackTime - delay
        currentTime = addUTCTime subTime startTime
    chapter <- loadChapter conn vid subTime
    comments <- loadComments conn vid currentTime uid
    let body = do
          div_ [class_ "header"] do
            for_ chapter $ div_ [class_ "chapter"] . toHtml
            pre_ [class_ "timestamp"] $ do
              toHtml $ formatTime defaultTimeLocale "%F %T" currentTime
              " ["
              toHtml $ formatTime defaultTimeLocale "%h:%2M:%2S" subTime
              when (delay /= 0) do
                " "
                when (delay > 0) "+"
                toHtml $ formatTime defaultTimeLocale "%1Es" delay
                "s"
              "]"
          ul_ [class_ "comments"] $
            -- TODO; render timestamp if far apart enough
            traverse_ fmtComment $
              reverse comments
    pure $
      View
        { title,
          content = fst $ evalRWS (renderTextT body) context mempty,
          scroll = True
        }
