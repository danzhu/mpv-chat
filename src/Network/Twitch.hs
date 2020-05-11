{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Twitch
  ( Auth(..)
  , Channel(..)
  , Video(..)
  , Emoticon(..)
  , Fragment(..)
  , Message(..)
  , Commenter(..)
  , Comment(..)
  , Comments(..)
  , emoteUrl
  , getVideo
  , sourceComments
  ) where

import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Aeson                     ( FromJSON )
import qualified Data.ByteString               as B
import           Data.Conduit                   ( ConduitT
                                                , yield
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Function                  ( (&) )
import           Data.Scientific                ( Scientific )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Simple            ( Query
                                                , addRequestHeader
                                                , getResponseBody
                                                , httpJSON
                                                , parseRequestThrow
                                                , setRequestQueryString
                                                )

type VideoId = T.Text

newtype Auth = Auth
  { clientId :: B.ByteString
  } deriving (Show)

newtype Channel = Channel
  { name :: T.Text
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

videoUrl :: VideoId -> T.Text
videoUrl vid = "https://api.twitch.tv/v5/videos/" <> vid

commentsUrl :: VideoId -> T.Text
commentsUrl vid = videoUrl vid <> "/comments"

emoteUrl :: T.Text -> T.Text
emoteUrl i = "//static-cdn.jtvnw.net/emoticons/v1/" <> i <> "/2.0"

query :: (MonadIO m, MonadThrow m, FromJSON a) => Auth -> T.Text -> Query -> m a
query auth url q = do
  req <- parseRequestThrow $ T.unpack url
  res <- httpJSON $ req
    & addRequestHeader "Client-ID" (clientId auth)
    & setRequestQueryString q
  pure $ getResponseBody res

getVideo :: Auth -> VideoId -> IO Video
getVideo auth vid = query auth (videoUrl vid) []

sourceComments :: Auth -> VideoId -> ConduitT i [Comment] IO ()
sourceComments auth vid = fetch "" where
  fetch cur = do
    cs <- query auth (commentsUrl vid) [("cursor", Just $ encodeUtf8 cur)]
    yield $ comments cs
    traverse_ fetch $ _next cs
