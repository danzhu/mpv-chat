module Network.Twitch.Channel
  ( Channel (..),
    ChannelId,
    parseChannelId,
    userChannel,
  )
where

import Data.Aeson (FromJSON)
import Network.Twitch.User (UserId)
import Text.Megaparsec
  ( Parsec,
    eof,
    runParser,
  )
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Read (read)

type Parser = Parsec Void Text

newtype ChannelId = ChannelId Int
  deriving newtype (Eq, FromJSON, Hashable, Ord, Show)

newtype Channel = Channel
  { _id :: ChannelId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)

-- TODO: find out if they are actually 1-to-1
userChannel :: UserId -> ChannelId
userChannel uid = ChannelId $ read $ show uid

parseChannelId :: Text -> Either String ChannelId
parseChannelId = left errorBundlePretty . runParser (p <* eof) ""
  where
    p :: Parser ChannelId
    p = ChannelId <$> decimal
