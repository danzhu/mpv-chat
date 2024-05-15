module MpvChat.Videos
  ( renderVideos,
  )
where

import Database.SQLite.Simple (Connection)
import Lucid.Base (ToHtml (toHtml), renderText)
import Lucid.Html5
  ( button_,
    class_,
    data_,
    li_,
    ul_,
  )
import MpvChat.Data (Video (Video), View (View))
import qualified MpvChat.Data
import MpvChat.Database (loadVideos)

renderVideos :: Connection -> IO View
renderVideos conn = do
  videos <- loadVideos conn
  let body = ul_ $
        for_ videos $ \Video {id, title} ->
          li_ $ do
            let url = "https://www.twitch.tv/videos/" <> tshow id
            button_
              [ class_ "load",
                data_ "post" "/loadfile",
                data_ "body" url
              ]
              "|>"
            " "
            toHtml title
  pure View {title = "Videos", content = renderText body, scroll = False}
