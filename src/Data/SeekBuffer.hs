module Data.SeekBuffer
  ( SeekBuffer
  , append
  , current
  , empty
  , future
  , past
  , seek
  ) where

data SeekBuffer k v = SeekBuffer
  { past :: [(k, v)]
  , future :: [(k, v)]
  , current :: k
  }
  deriving (Show)

empty :: k -> SeekBuffer k v
empty = SeekBuffer [] []

append :: Ord k => [(k, v)] -> SeekBuffer k v -> SeekBuffer k v
append as (SeekBuffer ps ns c) = seekForward $ SeekBuffer ps (ns <> as) c

seek :: Ord k => k -> SeekBuffer k v -> SeekBuffer k v
seek k sb = seekBackward $ seekForward sb { current = k }

seekForward :: Ord k => SeekBuffer k v -> SeekBuffer k v
seekForward (SeekBuffer ps (p@(k, _) : ns) c)
  | k < c = seekForward $ SeekBuffer (p : ps) ns c
seekForward sb = sb

seekBackward :: Ord k => SeekBuffer k v -> SeekBuffer k v
seekBackward (SeekBuffer (p@(k, _) : ps) ns c)
  | k >= c = seekBackward $ SeekBuffer ps (p : ns) c
seekBackward sb = sb
