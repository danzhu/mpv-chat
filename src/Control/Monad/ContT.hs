module Control.Monad.ContT
  ( contT_
  ) where

import           Control.Monad.Trans.Cont       ( ContT
                                                , mapContT
                                                )

contT_ :: (m r -> m r) -> ContT r m ()
contT_ f = mapContT f $ pure ()
