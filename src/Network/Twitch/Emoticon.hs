module Network.Twitch.Emoticon
  ( Emoticon (..),
  )
where

newtype Emoticon = Emoticon
  { emoticon_id :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, Hashable)
