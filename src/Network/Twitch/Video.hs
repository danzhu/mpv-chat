module Network.Twitch.Video
  ( Video(..)
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
  deriving newtype (Eq, FromJSON, Hashable, Ord, Show)

newtype Video = Video
  { channel :: Channel
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

parseVideoId :: Text -> Either String VideoId
parseVideoId = first errorBundlePretty . runParser (p <* eof) "" where
  p :: Parser VideoId
  p = VideoId <$> decimal
