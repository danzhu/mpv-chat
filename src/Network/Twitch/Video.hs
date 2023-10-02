module Network.Twitch.Video
  ( Images (..),
    Video (..),
    VideoId,
    parseVideoId,
  )
where

import Data.Aeson (FromJSON)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Network.Twitch.Channel (Channel)
import Text.Megaparsec
  ( Parsec,
    eof,
    runParser,
  )
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void Text

newtype VideoId = VideoId Int
  deriving newtype (Eq, Hashable, Ord, Show, FromField, ToField)

-- TODO: instance FromJSON VideoId

data Video = Video
  { title :: Text,
    views :: Int,
    url :: Text,
    published_at :: Maybe UTCTime,
    game :: Maybe Text,
    length :: NominalDiffTime,
    preview :: Images Text,
    channel :: Channel
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data Images a = Images
  { small :: a,
    medium :: a,
    large :: a,
    template :: a
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

parseVideoId :: Text -> Either String VideoId
parseVideoId = left errorBundlePretty . runParser (p <* eof) ""
  where
    p :: Parser VideoId
    p = VideoId <$> decimal
