module Network.Twitch.Twitch
  ( Auth(..)
  , Channel(..)
  , Comment(..)
  , Commenter(..)
  , Comments(..)
  , Emoticon(..)
  , Fragment(..)
  , Message(..)
  , Video(..)
  , VideoId
  , emoteUrl
  , getVideo
  , sourceComments
  ) where

import           Network.Request                ( request )

import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( FromJSON )
import qualified Data.ByteString               as B
import           Data.Conduit                   ( ConduitT
                                                , yield
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Scientific                ( Scientific )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Types             ( Query )

type VideoId = T.Text

newtype Auth = Auth
  { clientId :: B.ByteString
  } deriving (Show)

newtype Channel = Channel
  { _id :: Int
  } deriving (Generic, Show)

newtype Video = Video
  { channel :: Channel
  } deriving (Generic, Show)

newtype Emoticon = Emoticon
  { emoticon_id :: T.Text
  } deriving (Generic, Show)

data Fragment = Fragment
  { text     :: T.Text
  , emoticon :: Maybe Emoticon
  } deriving (Generic, Show)

data Message = Message
  { body       :: T.Text
  , fragments  :: [Fragment]
  , user_color :: Maybe T.Text
  } deriving (Generic, Show)

data Commenter = Commenter
  { name         :: T.Text
  , display_name :: T.Text
  , bio          :: Maybe T.Text
  } deriving (Generic, Show)

data Comment = Comment
  { message                :: Message
  , commenter              :: Commenter
  , content_offset_seconds :: Scientific
  } deriving (Generic, Show)

data Comments = Comments
  { comments :: [Comment]
  , _next    :: Maybe T.Text
  } deriving (Generic, Show)

instance FromJSON Channel
instance FromJSON Video
instance FromJSON Emoticon
instance FromJSON Fragment
instance FromJSON Message
instance FromJSON Commenter
instance FromJSON Comment
instance FromJSON Comments

rootUrl :: T.Text
rootUrl = "https://api.twitch.tv/v5"

videoUrl :: VideoId -> T.Text
videoUrl vid = rootUrl <> "/videos/" <> vid

commentsUrl :: VideoId -> T.Text
commentsUrl vid = videoUrl vid <> "/comments"

emoteUrl :: T.Text -> T.Text
emoteUrl i = "//static-cdn.jtvnw.net/emoticons/v1/" <> i <> "/2.0"

query :: (MonadIO m, MonadThrow m, FromJSON a) => Auth -> T.Text -> Query -> m a
query auth url q = request url q [("Client-ID", clientId auth)]

getVideo :: Auth -> VideoId -> IO Video
getVideo auth vid = query auth (videoUrl vid) []

sourceComments :: Auth -> VideoId -> ConduitT i [Comment] IO ()
sourceComments auth vid = fetch "" where
  fetch cur = do
    cs <- query auth (commentsUrl vid) [("cursor", Just $ encodeUtf8 cur)]
    yield $ comments cs
    traverse_ fetch $ _next cs
