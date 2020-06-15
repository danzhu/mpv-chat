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
  , parseVideoId
  , sourceComments
  ) where

import           Network.Request                ( request )

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( FromJSON )
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString               as B
import           Data.Char                      ( isDigit )
import           Data.Conduit                   ( ConduitT
                                                , yield
                                                )
import           Data.Foldable                  ( traverse_ )
import qualified Data.List.NonEmpty            as NE
import           Data.Scientific                ( Scientific )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Void                      ( Void )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Types             ( Query )
import           Text.Megaparsec                ( Parsec
                                                , runParser
                                                , takeWhile1P
                                                , eof
                                                )
import           Text.Megaparsec.Error          ( errorBundlePretty )

newtype VideoId = VideoId T.Text

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
  , fragments  :: NE.NonEmpty Fragment
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
  { comments :: NE.NonEmpty Comment
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
videoUrl (VideoId vid) = rootUrl <> "/videos/" <> vid

commentsUrl :: VideoId -> T.Text
commentsUrl vid = videoUrl vid <> "/comments"

emoteUrl :: T.Text -> T.Text
emoteUrl i = "//static-cdn.jtvnw.net/emoticons/v1/" <> i <> "/2.0"

query :: (MonadIO m, FromJSON a) => Auth -> T.Text -> Query -> m a
query auth url q = request url q [("Client-ID", clientId auth)]

parseVideoId :: T.Text -> Either String VideoId
parseVideoId = first errorBundlePretty . runParser (p <* eof) "" where
  p :: Parsec Void T.Text VideoId
  p = VideoId <$> takeWhile1P (Just "video id") isDigit

getVideo :: MonadIO m => Auth -> VideoId -> m Video
getVideo auth vid = query auth (videoUrl vid) []

sourceComments :: MonadIO m => Auth -> VideoId -> ConduitT i (NE.NonEmpty Comment) m ()
sourceComments auth vid = fetch "" where
  fetch cur = do
    cs <- query auth (commentsUrl vid) [("cursor", Just $ encodeUtf8 cur)]
    yield $ comments cs
    traverse_ fetch $ _next cs
