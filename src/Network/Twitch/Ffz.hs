{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Twitch.Ffz
  ( Emote(..)
  , Ffz(..)
  , Set(..)
  , channelUrl
  ) where

import           Data.Aeson                     ( FromJSON )
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           GHC.Generics                   ( Generic )

newtype Ffz = Ffz
  { sets :: HM.HashMap T.Text Set
  } deriving (Generic, Show)

newtype Set = Set
  { emoticons :: [Emote]
  } deriving (Generic, Show)

data Emote = Emote
  { name :: T.Text
  , urls :: HM.HashMap Int T.Text
  } deriving (Generic, Show)

instance FromJSON Ffz
instance FromJSON Set
instance FromJSON Emote

channelUrl :: T.Text -> T.Text
channelUrl c = "https://api.frankerfacez.com/v1/room/" <> c
