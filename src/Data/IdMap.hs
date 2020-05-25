module Data.IdMap
  ( Id
  , IdMap
  , empty
  , insert
  , lookup
  , remove
  ) where

import qualified Data.IntMap.Strict            as IM
import           Prelude                 hiding ( lookup )

type Id = Int

data IdMap a = IdMap Id (IM.IntMap a)
  deriving (Show)

empty :: Id -> IdMap a
empty i = IdMap i IM.empty

insert :: a -> IdMap a -> (Id, IdMap a)
insert a (IdMap i m) = (i, IdMap (succ i) $ IM.insert i a m)

lookup :: Id -> IdMap a -> Maybe a
lookup i (IdMap _ m) = IM.lookup i m

remove :: Id -> IdMap a -> (Maybe a, IdMap a)
remove i (IdMap i' m) = (a, IdMap i' m') where
  del _ _ = Nothing
  (a, m') = IM.updateLookupWithKey del i m
