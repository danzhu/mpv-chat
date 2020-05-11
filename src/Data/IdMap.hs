module Data.IdMap
  ( IdMap
  , empty
  , insert
  , lookup
  , remove
  ) where

import qualified Data.IntMap.Strict            as IM
import           Prelude                 hiding ( lookup )

data IdMap a = IdMap Int (IM.IntMap a)
  deriving (Show)

empty :: Int -> IdMap a
empty i = IdMap i IM.empty

insert :: a -> IdMap a -> (Int, IdMap a)
insert a (IdMap i m) = (i, IdMap (i + 1) $ IM.insert i a m)

lookup :: Int -> IdMap a -> Maybe a
lookup i (IdMap _ m) = IM.lookup i m

remove :: Int -> IdMap a -> (Maybe a, IdMap a)
remove i (IdMap i' m) = (a, IdMap i' m') where
  del _ _ = Nothing
  (a, m') = IM.updateLookupWithKey del i m
