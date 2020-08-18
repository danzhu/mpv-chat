module Network.Twitch.Channel
  ( Channel(..)
  , ChannelId
  , parseChannelId
  ) where

import           MpvChat.Prelude
import           Text.Megaparsec                ( Parsec
                                                , eof
                                                , runParser
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Text.Megaparsec.Error          ( errorBundlePretty )

type Parser = Parsec Void Text

newtype ChannelId = ChannelId Int
  deriving newtype (Eq, FromJSON, Hashable, Ord, Show)

newtype Channel = Channel
  { _id :: ChannelId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

parseChannelId :: Text -> Either String ChannelId
parseChannelId = first errorBundlePretty . runParser (p <* eof) "" where
  p :: Parser ChannelId
  p = ChannelId <$> decimal
