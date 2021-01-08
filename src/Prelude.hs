-- |
-- Custom Prelude.
module Prelude
  ( -- * Re-exports
    module E,

    -- * Type Aliases
    LByteString,
    LText,

    -- * Function Synonyms
    identity,
    map,
    sequence,
    undef,

    -- * Utility Functions
    tshow,
  )
where

import Control.Applicative as E
  ( Alternative (empty, many, some, (<|>)),
    Applicative (liftA2, pure, (*>), (<*), (<*>)),
  )
import Control.Arrow as E
  ( Arrow (arr, first, second, (&&&), (***)),
    ArrowApply,
    ArrowChoice (left, right, (+++), (|||)),
    ArrowLoop,
    ArrowMonad (ArrowMonad),
    ArrowPlus ((<+>)),
    ArrowZero (zeroArrow),
    Kleisli (Kleisli, runKleisli),
    returnA,
    (<<<),
    (<<^),
    (>>>),
    (>>^),
    (^<<),
    (^>>),
  )
import Control.Category (id)
import Control.Category as E (Category ((.)))
import Control.Concurrent.STM.TVar as E (stateTVar)
import Control.Monad as E
  ( Monad (return, (>>=)),
    MonadFail (fail),
    MonadPlus,
    forever,
    guard,
    join,
    unless,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.IO.Class as E (MonadIO (liftIO))
import Control.Monad.IO.Unlift as E (MonadUnliftIO (withRunInIO))
import Control.Monad.RWS.Class as E
  ( MonadRWS,
    MonadReader (ask, local, reader),
    MonadState (get, put, state),
    MonadWriter (listen, pass, tell, writer),
    asks,
    censor,
    gets,
    listens,
    modify,
    modify',
  )
import Control.Monad.Trans as E (MonadTrans (lift))
import Control.Monad.Trans.Cont as E
  ( Cont,
    ContT (ContT, runContT),
    evalContT,
    mapCont,
    mapContT,
    runCont,
    withCont,
    withContT,
  )
import Control.Monad.Trans.Except as E (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans.Identity as E (IdentityT (IdentityT, runIdentityT))
import Control.Monad.Trans.Maybe as E (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.RWS as E
  ( RWS,
    RWST (RWST, runRWST),
    evalRWS,
    evalRWST,
    execRWS,
    execRWST,
    runRWS,
  )
import Control.Monad.Trans.Reader as E
  ( Reader,
    ReaderT (ReaderT, runReaderT),
    runReader,
  )
import Control.Monad.Trans.State as E
  ( State,
    StateT (StateT, runStateT),
    runState,
  )
import Control.Monad.Trans.Writer as E
  ( Writer,
    WriterT (WriterT, runWriterT),
    runWriter,
  )
import Data.Bifunctor as E (Bifunctor (bimap))
import Data.Bool as E (Bool (False, True), bool, not, otherwise, (&&), (||))
import Data.ByteString as E (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Char as E (Char)
import Data.Containers as E
  ( BiPolyMap (mapKeysWith),
    HasKeysSet (keysSet),
    IsMap
      ( adjustMap,
        adjustWithKey,
        alterMap,
        deleteMap,
        filterMap,
        findWithDefault,
        insertLookupWithKey,
        insertMap,
        insertWith,
        insertWithKey,
        lookup,
        mapFromList,
        mapWithKey,
        singletonMap,
        unionWith,
        unionWithKey,
        unionsWith,
        updateLookupWithKey,
        updateMap,
        updateWithKey
      ),
    IsSet (deleteSet, filterSet, insertSet, setFromList),
    PolyMap (differenceMap, intersectionMap, intersectionWithMap),
    SetContainer
      ( difference,
        intersection,
        keys,
        member,
        notMember,
        union,
        unions
      ),
  )
import Data.Default.Class as E (Default (def))
import Data.Either as E (Either (Left, Right), either)
import Data.Eq as E (Eq ((/=), (==)))
import Data.Foldable as E (Foldable, asum)
import Data.Function as E (const, flip, ($), (&))
import Data.Functor (Functor (fmap))
import Data.Functor as E (Functor ((<$)), void, ($>), (<$>))
import Data.Functor.Constant as E (Constant (Constant, getConstant))
import Data.Functor.Identity as E (Identity (Identity, runIdentity))
import Data.HashMap.Strict as E (HashMap)
import Data.HashSet as E (HashSet)
import Data.Hashable as E (Hashable)
import Data.Int as E (Int)
import Data.IntMap.Strict as E (IntMap)
import Data.IntSet as E (IntSet)
import Data.List as E ((++))
import Data.List.NonEmpty as E (NonEmpty)
import Data.Map.Strict as E (Map)
import Data.Maybe as E (Maybe (Just, Nothing), fromMaybe, isJust, maybe)
import Data.MonoTraversable as E
  ( Element,
    MonoFoldable,
    MonoFunctor (omap),
    MonoPointed,
    MonoTraversable (otraverse),
    headMay,
    lastMay,
  )
import Data.MonoTraversable.Unprefixed as E
  ( all,
    and,
    any,
    elem,
    fold,
    foldMap,
    foldl',
    foldr,
    for_,
    intercalate,
    length,
    notElem,
    null,
    or,
    point,
    product,
    sequence_,
    sum,
    toList,
    traverse_,
  )
import Data.Monoid as E (Monoid (mempty))
import Data.Ord as E
  ( Ord (compare, max, min, (<), (<=), (>), (>=)),
    Ordering (EQ, GT, LT),
  )
import Data.Proxy as E (Proxy (Proxy))
import Data.Scientific as E (Scientific)
import Data.Semigroup as E (Semigroup ((<>)))
import Data.Sequence as E (Seq)
import Data.Sequences as E
  ( IsSequence
      ( break,
        drop,
        dropEnd,
        dropWhile,
        filter,
        filterM,
        fromList,
        groupBy,
        partition,
        replicate,
        replicateM,
        span,
        splitAt,
        splitWhen,
        take,
        takeWhile,
        uncons,
        unsnoc
      ),
    LazySequence (fromStrict, toStrict),
    SemiSequence (cons, find, intersperse, reverse, snoc, sortBy),
    Textual (lines, toCaseFold, toLower, toUpper, unlines, unwords, words),
    Utf8 (decodeUtf8, encodeUtf8),
    catMaybes,
    delete,
    deleteBy,
    dropPrefix,
    dropSuffix,
    ensurePrefix,
    ensureSuffix,
    group,
    groupAll,
    isInfixOf,
    isPrefixOf,
    isSuffixOf,
    pack,
    repack,
    replaceSeq,
    sort,
    sortOn,
    splitElem,
    splitSeq,
    stripPrefix,
    stripSuffix,
    unpack,
  )
import Data.Set as E (Set)
import Data.String as E (IsString, String)
import Data.Text as E (Text)
import qualified Data.Text.Lazy as L
import Data.Traversable (Traversable (sequenceA))
import Data.Traversable as E (Traversable (traverse), for)
import Data.Tuple as E (curry, fst, snd, swap, uncurry)
import Data.Void as E (Void, absurd)
import GHC.Enum as E
  ( Bounded (maxBound, minBound),
    Enum (pred, succ),
  )
import GHC.Err (undefined)
import GHC.Err as E (error)
import GHC.Float as E (Float)
import GHC.Generics as E (Generic)
import GHC.Num as E
  ( Integer,
    Num (abs, negate, signum, (*), (+), (-)),
    subtract,
  )
import GHC.Real as E (RealFrac (ceiling, floor, round, truncate))
import GHC.Stack as E (HasCallStack)
import Lens.Micro as E
  ( (%~),
    (.~),
    (?~),
    (^.),
    (^?),
    _Just,
    _Left,
    _Nothing,
    _Right,
    _head,
    _init,
    _last,
    _tail,
  )
import Lens.Micro.TH as E (makeLenses)
import System.IO as E (FilePath, IO)
import Text.Read as E (Read, readMaybe)
import Text.Show as E (Show (show))
import UnliftIO.Async as E (concurrently, concurrently_, mapConcurrently)
import UnliftIO.Exception as E
  ( Exception,
    IOException,
    bracket,
    finally,
    try,
    tryJust,
  )
import UnliftIO.STM as E
  ( TBQueue,
    TChan,
    TMVar,
    TVar,
    atomically,
    dupTChan,
    modifyTVar',
    newBroadcastTChanIO,
    newEmptyTMVarIO,
    newTBQueueIO,
    newTChanIO,
    newTVarIO,
    putTMVar,
    readTBQueue,
    readTChan,
    readTMVar,
    readTVar,
    readTVarIO,
    registerDelay,
    swapTVar,
    writeTBQueue,
    writeTChan,
    writeTVar,
  )

-- | Synonym for lazy `L.ByteString`.
type LByteString = L.ByteString

-- | Synonym for lazy `L.Text`.
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

-- | `Text`-returning `show`.
tshow :: Show a => a -> Text
tshow = fromList . show
