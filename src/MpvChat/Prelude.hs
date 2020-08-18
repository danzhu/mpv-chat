-- |
-- Custom Prelude.

module MpvChat.Prelude
  (
  -- * Re-exports
    module E
  -- * Type Aliases
  , LByteString
  , LHashMap
  , LMap
  , LText
  -- * Function Synonyms
  , identity
  , map
  , sequence
  , undef
  )
where

import           Control.Applicative           as E
                                                ( Alternative
                                                  ( empty
                                                  , many
                                                  , some
                                                  , (<|>)
                                                  )
                                                , Applicative
                                                  ( liftA2
                                                  , pure
                                                  , (*>)
                                                  , (<*)
                                                  , (<*>)
                                                  )
                                                )
import           Control.Category               ( id )
import           Control.Category              as E
                                                ( Category((.)) )
import           Control.Concurrent.STM.TVar   as E
                                                ( stateTVar )
import           Control.Monad                 as E
                                                ( Monad((>>=))
                                                , forever
                                                , guard
                                                , join
                                                , unless
                                                , when
                                                , (<=<)
                                                , (=<<)
                                                )
import           Control.Monad.IO.Class        as E
                                                ( MonadIO(liftIO) )
import           Control.Monad.IO.Unlift       as E
                                                ( MonadUnliftIO(withRunInIO) )
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
                                                , RWST(RWST)
                                                , evalRWS
                                                , evalRWST
                                                , execRWS
                                                , execRWST
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
                                                ( Bool(False, True)
                                                , bool
                                                , not
                                                , otherwise
                                                , (&&)
                                                , (||)
                                                )
import           Data.ByteString               as E
                                                ( ByteString )
import qualified Data.ByteString.Lazy          as L
import           Data.Char                     as E
                                                ( Char )
import           Data.Conduit                  as E
                                                ( ConduitT
                                                , runConduit
                                                , yield
                                                , (.|)
                                                )
import           Data.Containers               as E
                                                ( IsMap(..)
                                                , IsSet(..)
                                                , SetContainer(..)
                                                )
import           Data.Default.Class            as E
                                                ( Default(def) )
import           Data.Either                   as E
                                                ( Either(Left, Right)
                                                , either
                                                )
import           Data.Eq                       as E
                                                ( Eq((==), (/=)) )
import           Data.Foldable                 as E
                                                ( Foldable
                                                , asum
                                                )
import           Data.Function                 as E
                                                ( const
                                                , flip
                                                , ($)
                                                , (&)
                                                )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Functor                  as E
                                                ( Functor
                                                , (<$>)
                                                )
import           Data.Functor.Identity         as E
                                                ( Identity(Identity)
                                                , runIdentity
                                                )
import qualified Data.HashMap.Lazy             as L
import           Data.HashMap.Strict           as E
                                                ( HashMap )
import           Data.HashSet                  as E
                                                ( HashSet )
import           Data.Hashable                 as E
                                                ( Hashable )
import           Data.Int                      as E
                                                ( Int )
import           Data.IntMap.Strict            as E
                                                ( IntMap )
import           Data.List.NonEmpty            as E
                                                ( NonEmpty )
import qualified Data.Map.Lazy                 as L
import           Data.Map.Strict               as E
                                                ( Map )
import           Data.Maybe                    as E
                                                ( Maybe(Just, Nothing)
                                                , fromMaybe
                                                , isJust
                                                , maybe
                                                )
import           Data.MonoTraversable          as E
                                                ( Element
                                                , MonoFoldable
                                                , MonoFunctor(omap)
                                                , MonoTraversable(otraverse)
                                                )
import           Data.MonoTraversable.Unprefixed
                                               as E
                                                ( all
                                                , and
                                                , any
                                                , elem
                                                , fold
                                                , foldMap
                                                , foldl'
                                                , foldr
                                                , for_
                                                , intercalate
                                                , length
                                                , notElem
                                                , null
                                                , or
                                                , point
                                                , product
                                                , sequence_
                                                , sum
                                                , toList
                                                , traverse_
                                                )
import           Data.Monoid                   as E
                                                ( Monoid(mempty) )
import           Data.Ord                      as E
                                                ( Ord(..)
                                                , Ordering(EQ, GT, LT)
                                                )
import           Data.Scientific               as E
                                                ( Scientific )
import           Data.Semigroup                as E
                                                ( Semigroup((<>)) )
import           Data.Sequences                as E
                                                ( IsSequence(..)
                                                , LazySequence(..)
                                                , SemiSequence(..)
                                                , Textual(..)
                                                , Utf8(..)
                                                , catMaybes
                                                , delete
                                                , deleteBy
                                                , dropPrefix
                                                , dropSuffix
                                                , ensurePrefix
                                                , ensureSuffix
                                                , group
                                                , groupAll
                                                , isInfixOf
                                                , isPrefixOf
                                                , isSuffixOf
                                                , replaceSeq
                                                , sort
                                                , sortOn
                                                , splitElem
                                                , splitSeq
                                                , stripPrefix
                                                , stripSuffix
                                                )
import           Data.String                   as E
                                                ( String )
import           Data.Text                     as E
                                                ( Text )
import qualified Data.Text.Lazy                as L
import           Data.Traversable               ( Traversable(sequenceA) )
import           Data.Traversable              as E
                                                ( Traversable(traverse)
                                                , for
                                                )
import           Data.Tuple                    as E
                                                ( curry
                                                , fst
                                                , snd
                                                , uncurry
                                                )
import           Data.Void                     as E
                                                ( Void
                                                , absurd
                                                )
import           GHC.Enum                      as E
                                                ( Enum(..)
                                                , Bounded(maxBound, minBound)
                                                )
import           GHC.Err                        ( undefined )
import           GHC.Err                       as E
                                                ( error )
import           GHC.Float                     as E
                                                ( Float )
import           GHC.Generics                  as E
                                                ( Generic )
import           GHC.Num                       as E
                                                ( Integer
                                                , Num(..)
                                                , subtract
                                                )
import           GHC.Real                      as E
                                                ( RealFrac(..) )
import           GHC.Stack                     as E
                                                ( HasCallStack )
import           Lens.Micro                    as E
                                                ( (%~)
                                                , (.~)
                                                , (?~)
                                                , (^.)
                                                , _Just
                                                , _last
                                                )
import           Lens.Micro.TH                 as E
                                                ( makeLenses )
import           System.IO                     as E
                                                ( FilePath
                                                , IO
                                                )
import           Text.Read                     as E
                                                ( Read )
import           Text.Show                     as E
                                                ( Show(show) )
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

type LByteString = L.ByteString
type LHashMap = L.HashMap
type LMap = L.Map
type LText = L.Text

-- | Synonym for `id`.
identity :: Category cat => cat a a
identity = id

-- | Synonym for `fmap`.
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

-- | Synonym for `sequenceA`.
sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = sequenceA

-- | Synonym for `undefined` with warning.
undef :: HasCallStack => a
undef = undefined
{-# WARNING undef "'undef' left in code" #-}
