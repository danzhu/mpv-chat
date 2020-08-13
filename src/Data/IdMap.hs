module Data.IdMap
  ( Id
  , IdMap
  , empty
  , insert
  , lookup
  , remove
  ) where

import qualified MpvChat.Prelude               as P
import           MpvChat.Prelude         hiding ( empty
                                                , lookup
                                                )

type Id = Int

data IdMap a = IdMap Id (IntMap a)
  deriving stock Show

empty :: Id -> IdMap a
empty i = IdMap i mempty

insert :: a -> IdMap a -> (Id, IdMap a)
insert a (IdMap i m) = (i, IdMap (succ i) $ insertMap i a m)

lookup :: Id -> IdMap a -> Maybe a
lookup i (IdMap _ m) = P.lookup i m

remove :: Id -> IdMap a -> (Maybe a, IdMap a)
remove i (IdMap i' m) = (a, IdMap i' m') where
  del _ _ = Nothing
  (a, m') = updateLookupWithKey del i m
