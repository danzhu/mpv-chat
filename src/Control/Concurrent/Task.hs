module Control.Concurrent.Task
  ( Task
  , getTask
  , killTask
  , runTask
  , withTask
  ) where

import           Control.Concurrent.Async       ( Async
                                                , async
                                                , cancel
                                                , link
                                                , uninterruptibleCancel
                                                )
import           Control.Concurrent.STM.TVar    ( TVar
                                                , newTVarIO
                                                , readTVarIO
                                                , swapTVar
                                                )
import           Control.Concurrent.STM.TMVar   ( newEmptyTMVarIO
                                                , putTMVar
                                                , takeTMVar
                                                )
import           Control.Exception              ( bracket )
import           Data.Foldable                  ( traverse_ )
import           Control.Monad.STM              ( atomically )

newtype Task = Task { getTask :: TVar (Maybe (Async ())) }

replace :: Task -> Maybe (Async ()) -> IO ()
replace (Task v) n = do
  t <- atomically $ swapTVar v n
  traverse_ cancel t

withTask :: (Task -> IO ()) -> IO ()
withTask = bracket open close where
  open = Task <$> newTVarIO Nothing
  close (Task v) = do
    t <- readTVarIO v
    traverse_ uninterruptibleCancel t

runTask :: Task -> IO () -> IO ()
runTask t f = do
  w <- newEmptyTMVarIO
  a <- async $ atomically (takeTMVar w) *> f
  link a
  replace t $ Just a
  atomically $ putTMVar w ()

killTask :: Task -> IO ()
killTask t = replace t Nothing
