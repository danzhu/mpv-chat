module Control.Concurrent.Task
  ( Task
  , killTask
  , newEmptyTask
  , newTask
  , startTask
  , stopTask
  , withEmptyTask
  , withTask
  , withTask_
  ) where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Data.Foldable                  ( traverse_ )
import           UnliftIO.Async                 ( Async
                                                , async
                                                , cancel
                                                , link
                                                , uninterruptibleCancel
                                                )
import           UnliftIO.Exception             ( bracket )
import           UnliftIO.STM                   ( TVar
                                                , atomically
                                                , newEmptyTMVarIO
                                                , newTVarIO
                                                , putTMVar
                                                , readTMVar
                                                , readTVarIO
                                                , swapTVar
                                                )

newtype Task = Task (TVar (Maybe (Async ())))

replace :: MonadIO m => Task -> Maybe (Async ()) -> m ()
replace (Task v) n = do
  t <- atomically $ swapTVar v n
  traverse_ cancel t

newEmptyTask :: MonadIO m => m Task
newEmptyTask = Task <$> newTVarIO Nothing

newTask :: MonadUnliftIO m => m () -> m Task
newTask f = do
  a <- async f
  link a
  Task <$> newTVarIO (Just a)

killTask :: MonadIO m => Task -> m ()
killTask (Task v) = do
  t <- readTVarIO v
  traverse_ uninterruptibleCancel t

withEmptyTask :: MonadUnliftIO m => (Task -> m a) -> m a
withEmptyTask = bracket newEmptyTask killTask

withTask :: MonadUnliftIO m => m () -> (Task -> m a) -> m a
withTask f = bracket (newTask f) killTask

withTask_ :: MonadUnliftIO m => m () -> m a -> m a
withTask_ f = withTask f . const

startTask :: MonadUnliftIO m => Task -> m () -> m ()
startTask t f = do
  w <- newEmptyTMVarIO
  a <- async $ atomically (readTMVar w) *> f
  link a
  replace t $ Just a
  atomically $ putTMVar w ()

stopTask :: MonadIO m => Task -> m ()
stopTask t = replace t Nothing
