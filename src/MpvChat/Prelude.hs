module MpvChat.Prelude
  ( module E
  )
where

import           Control.Applicative           as E
                                                ( many
                                                , some
                                                , (<|>)
                                                )
import           Control.Concurrent.STM.TVar   as E
                                                ( stateTVar )
import           Control.Monad                 as E
                                                ( forever
                                                , guard
                                                , join
                                                , unless
                                                , when
                                                , (<=<)
                                                )
import           Control.Monad.IO.Class        as E
                                                ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.IO.Unlift       as E
                                                ( MonadUnliftIO
                                                , withRunInIO
                                                )
import           Control.Monad.Trans           as E
                                                ( lift )
import           Control.Monad.Trans.Cont      as E
                                                ( Cont
                                                , ContT(ContT)
                                                , evalContT
                                                , mapCont
                                                , mapContT
                                                , runCont
                                                , runContT
                                                , withCont
                                                , withContT
                                                )
import           Control.Monad.Trans.RWS       as E
                                                ( RWS
                                                , RWST
                                                , execRWS
                                                , execRWST
                                                , evalRWS
                                                , evalRWST
                                                , runRWS
                                                , runRWST
                                                )
import           Control.Monad.Trans.Reader    as E
                                                ( Reader
                                                , ReaderT(ReaderT)
                                                , runReader
                                                , runReaderT
                                                )
import           Control.Monad.Trans.State     as E
                                                ( State
                                                , StateT(StateT)
                                                , runState
                                                , runStateT
                                                )
import           Control.Monad.Trans.Writer    as E
                                                ( Writer
                                                , WriterT(WriterT)
                                                , runWriter
                                                , runWriterT
                                                )
import           Data.Aeson                    as E
                                                ( FromJSON
                                                , ToJSON
                                                )
import           Data.Bifunctor                as E
                                                ( Bifunctor
                                                , first
                                                , second
                                                )
import           Data.Bool                     as E
                                                ( bool )
import           Data.Conduit                  as E
                                                ( ConduitT
                                                , runConduit
                                                , yield
                                                , (.|)
                                                )
import           Data.Default.Class            as E
                                                ( Default
                                                , def
                                                )
import           Data.Foldable                 as E
                                                ( asum
                                                , fold
                                                , for_
                                                , sequenceA_
                                                , traverse_
                                                )
import           Data.Function                 as E
                                                ( (&) )
import           Data.Functor.Identity         as E
                                                ( Identity(Identity)
                                                , runIdentity
                                                )
import           Data.HashSet                  as E
                                                ( HashSet )
import           Data.Hashable                 as E
                                                ( Hashable )
import           Data.List.NonEmpty            as E
                                                ( NonEmpty )
import           Data.Maybe                    as E
                                                ( fromMaybe
                                                , isJust
                                                )
import           Data.Scientific               as E
                                                ( Scientific )
import           Data.Traversable              as E
                                                ( for )
import           Data.Void                     as E
                                                ( Void
                                                , absurd
                                                )
import           GHC.Exts                      as E
                                                ( IsList
                                                , Item
                                                , fromList
                                                , toList
                                                )
import           GHC.Generics                  as E
                                                ( Generic )
import           Lens.Micro                    as E
                                                ( _Just
                                                , _last
                                                , (%~)
                                                , (.~)
                                                , (?~)
                                                , (^.)
                                                )
import           Lens.Micro.TH                 as E
                                                ( makeLenses )
import           Prelude                       as E
                                         hiding ( filter
                                                , head
                                                , init
                                                , last
                                                , lookup
                                                , map
                                                , reverse
                                                , tail
                                                )
import           UnliftIO.Async                as E
                                                ( concurrently_
                                                , mapConcurrently
                                                )
import           UnliftIO.Exception            as E
                                                ( Exception
                                                , IOException
                                                , bracket
                                                , try
                                                , tryJust
                                                )
import           UnliftIO.STM                  as E
                                                ( TBQueue
                                                , TChan
                                                , TMVar
                                                , TVar
                                                , atomically
                                                , dupTChan
                                                , modifyTVar'
                                                , newBroadcastTChanIO
                                                , newEmptyTMVarIO
                                                , newTBQueueIO
                                                , newTChanIO
                                                , newTVarIO
                                                , putTMVar
                                                , readTBQueue
                                                , readTChan
                                                , readTMVar
                                                , readTVar
                                                , readTVarIO
                                                , registerDelay
                                                , swapTVar
                                                , writeTBQueue
                                                , writeTChan
                                                , writeTVar
                                                )
