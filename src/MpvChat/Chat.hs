module MpvChat.Chat
  ( renderChat,
  )
where

import Data.Time.Clock (addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple (Connection)
import Lucid.Base
  ( HtmlT,
    renderBST,
    renderText,
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
  ( ChatState (ChatState),
    Comment (Comment),
    Highlight (Highlight, NoHighlight),
    User (User),
    Video (Video),
    View (View),
  )
import qualified MpvChat.Data
import MpvChat.Database (loadChapter, loadComments)
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

renderChat :: Connection -> ChatState -> IO View
renderChat
  conn
  ChatState
    { video = Just Video {id = vid, title, createdAt = startTime, emotes},
      playbackTime = Just playbackTime,
      delay
    } = do
    let subTime = playbackTime - delay
        currentTime = addUTCTime subTime startTime
    chapter <- loadChapter conn vid subTime
    comments <- loadComments conn vid currentTime
    let body = do
          for_ chapter $ div_ [class_ "chapter"] . toHtml
          pre_ $ do
            toHtml $ formatTime defaultTimeLocale "%F %T" currentTime
            " ["
            toHtml $ formatTime defaultTimeLocale "%h:%2M:%2S" subTime
            when (delay /= 0) do
              " "
              when (delay > 0) "+"
              toHtml $ tshow delay
            "] "
          ul_ [class_ "comments"] $
            for_ comments $
              toHtmlRaw . flip runReader emotes . renderBST . fmtComment
    pure $ View title $ renderText body
renderChat _ _ = pure $ View "Chat" $ renderText $ pre_ "idle"
