module Control.Concurrent.Task
  ( Task,
    startTask,
    stopTask,
    withEmptyTask,
    withTask_,
  )
where

import UnliftIO.Async
  ( Async,
    async,
    link,
    uninterruptibleCancel,
    withAsync,
  )
import UnliftIO.Exception (mask, mask_)

newtype Task = Task (TVar (Maybe (Async ())))

replace :: (MonadIO m) => Task -> Maybe (Async ()) -> m ()
replace (Task v) n = do
  t <- atomically $ swapTVar v n
  traverse_ uninterruptibleCancel t

newEmptyTask :: (MonadIO m) => m Task
newEmptyTask = Task <$> newTVarIO Nothing

killTask :: (MonadIO m) => Task -> m ()
killTask (Task v) = do
  t <- readTVarIO v
  traverse_ uninterruptibleCancel t

withEmptyTask :: (MonadUnliftIO m) => (Task -> m a) -> m a
withEmptyTask = bracket newEmptyTask killTask

withTask_ :: (MonadUnliftIO m) => m () -> m a -> m a
withTask_ action inner = withAsync action $ \a -> link a *> inner

startTask :: (MonadUnliftIO m) => Task -> m () -> m ()
startTask t f = do
  w <- newTVarIO False
  mask $ \restore -> do
    a <- async $ restore $ atomically (guard =<< readTVar w) *> f
    replace t $ Just a
    atomically $ writeTVar w True
    restore $ link a

stopTask :: (MonadIO m) => Task -> m ()
stopTask t = liftIO $ mask_ $ replace t Nothing
