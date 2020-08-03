module Control.Monad.ContT
  ( contT_
  , runContT_
  ) where

import           Control.Monad.Trans.Cont       ( ContT
                                                , mapContT
                                                , runContT
                                                )

contT_ :: (m r -> m r) -> ContT r m ()
contT_ f = mapContT f $ pure ()

runContT_ :: ContT r m a -> m r -> m r
runContT_ c = runContT c . const

-- TODO: maybe unliftio versions of shift/reset?
