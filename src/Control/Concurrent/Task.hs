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

import           Control.Concurrent.Async       ( Async
                                                , async
                                                , cancel
                                                , link
                                                , uninterruptibleCancel
                                                )
import           Control.Concurrent.STM.TMVar   ( newEmptyTMVarIO
                                                , putTMVar
                                                , readTMVar
                                                )
import           Control.Concurrent.STM.TVar    ( TVar
                                                , newTVarIO
                                                , readTVarIO
                                                , swapTVar
                                                )
import           Control.Exception              ( bracket )
import           Control.Monad.STM              ( atomically )
import           Data.Foldable                  ( traverse_ )

newtype Task = Task (TVar (Maybe (Async ())))

replace :: Task -> Maybe (Async ()) -> IO ()
replace (Task v) n = do
  t <- atomically $ swapTVar v n
  traverse_ cancel t

newEmptyTask :: IO Task
newEmptyTask = Task <$> newTVarIO Nothing

newTask :: IO () -> IO Task
newTask f = do
  a <- async f
  link a
  Task <$> newTVarIO (Just a)

killTask :: Task -> IO ()
killTask (Task v) = do
  t <- readTVarIO v
  traverse_ uninterruptibleCancel t

withEmptyTask :: (Task -> IO a) -> IO a
withEmptyTask = bracket newEmptyTask killTask

withTask :: IO () -> (Task -> IO a) -> IO a
withTask f = bracket (newTask f) killTask

withTask_ :: IO () -> IO a -> IO a
withTask_ f = withTask f . const

startTask :: Task -> IO () -> IO ()
startTask t f = do
  w <- newEmptyTMVarIO
  a <- async $ atomically (readTMVar w) *> f
  link a
  replace t $ Just a
  atomically $ putTMVar w ()

stopTask :: Task -> IO ()
stopTask t = replace t Nothing
