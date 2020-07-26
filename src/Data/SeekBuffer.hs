module Data.SeekBuffer
  ( SeekBuffer
  , append
  , empty
  , future
  , past
  , seek
  ) where

data SeekBuffer a = SeekBuffer
  { past :: [a]
  , future :: [a]
  }
  deriving stock Show

empty :: SeekBuffer a
empty = SeekBuffer [] []

append :: [a] -> SeekBuffer a -> SeekBuffer a
append as (SeekBuffer ps ns) = SeekBuffer ps $ ns <> as

seek :: (a -> Bool) -> SeekBuffer a -> SeekBuffer a
seek adv sb = seekBackward adv $ seekForward adv sb

seekForward :: (a -> Bool) -> SeekBuffer a -> SeekBuffer a
seekForward adv (SeekBuffer ps (c : ns))
  | adv c = seekForward adv $ SeekBuffer (c : ps) ns
seekForward _ sb = sb

seekBackward :: (a -> Bool) -> SeekBuffer a -> SeekBuffer a
seekBackward adv (SeekBuffer (c : ps) ns)
  | not $ adv c = seekBackward adv $ SeekBuffer ps (c : ns)
seekBackward _ sb = sb
