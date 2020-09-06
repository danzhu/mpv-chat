module Network.Twitch.Video
  ( Images(..)
  , Video(..)
  , VideoId
  , parseVideoId
  ) where

import           MpvChat.Prelude
import           Network.Twitch.Channel         ( Channel )
import           Text.Megaparsec                ( Parsec
                                                , eof
                                                , runParser
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Text.Megaparsec.Error          ( errorBundlePretty )

type Parser = Parsec Void Text

newtype VideoId = VideoId Integer
  deriving newtype (Eq, Hashable, Ord, Show)

-- TODO: instance FromJSON VideoId

data Video = Video
  { title :: Text
  , views :: Int
  , url :: Text
  , game :: Maybe Text
  , length :: Int
  , preview :: Images Text
  , channel :: Channel
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

data Images a = Images
  { small :: a
  , medium :: a
  , large :: a
  , template :: a
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

parseVideoId :: Text -> Either String VideoId
parseVideoId = first errorBundlePretty . runParser (p <* eof) "" where
  p :: Parser VideoId
  p = VideoId <$> decimal
