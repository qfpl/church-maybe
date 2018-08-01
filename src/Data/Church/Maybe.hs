{-# language NoImplicitPrelude #-}
{-# language RankNTypes #-}

module Data.Church.Maybe
  ( Maybe(..), just, nothing, maybe, isNothing, isJust, fromMaybe
  , listToMaybe, maybeToList
  , catMaybes, mapMaybe
  )
where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad (Monad(..), MonadPlus(..), liftM2)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))
import Data.Bool (Bool(..))
import Data.Foldable (Foldable(..))
import Data.Function ((.), const, id)
import Data.Functor (Functor(..))
import Data.Functor.Alt (Alt(..))
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Traversable (Traversable(..))
import GHC.Err (errorWithoutStackTrace)

newtype Maybe a = Maybe { unMaybe :: forall r. r -> (a -> r) -> r }

{-# inline just #-}
just :: a -> Maybe a
just a = Maybe (\_ f -> f a)

{-# inline nothing #-}
nothing :: Maybe a
nothing = Maybe (\a _ -> a)

{-# inline maybe #-}
maybe :: b -> (a -> b) -> Maybe a -> b
maybe b f m = unMaybe m b f

{-# inline isNothing #-}
isNothing :: Maybe a -> Bool
isNothing m = unMaybe m True (const False)

{-# inline isJust #-}
isJust :: Maybe a -> Bool
isJust m = unMaybe m False (const True)

{-# inline fromMaybe #-}
fromMaybe :: a -> Maybe a -> a
fromMaybe a m = unMaybe m a id

{-# inline listToMaybe #-}
listToMaybe :: [a] -> Maybe a
listToMaybe [] = nothing
listToMaybe (a:_) = just a

{-# inline maybeToList #-}
maybeToList :: Maybe a -> [a]
maybeToList m = unMaybe m [] (: [])

{-# inline catMaybes #-}
catMaybes :: [Maybe a] -> [a]
catMaybes = go
  where
    go [] = []
    go (a : as) = unMaybe a (go as) (: go as)

{-# inline mapMaybe #-}
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = go
  where
    go [] = []
    go (a : as) = unMaybe (f a) (go as) (: go as)

instance Functor Maybe where
  {-# inline fmap #-}
  fmap f (Maybe m) = Maybe (\n j -> m n (j . f))

instance Apply Maybe where
  {-# inline (<.>) #-}
  Maybe mf <.> Maybe ma = Maybe (\n j -> mf n (\f -> ma n (j . f)))

instance Applicative Maybe where
  {-# inline pure #-}
  pure = just
  {-# inline (<*>) #-}
  (<*>) = (<.>)

instance Alt Maybe where
  {-# inline (<!> )#-}
  Maybe ma <!> Maybe mb = Maybe (\n j -> ma (mb n j) j)

instance Alternative Maybe where
  {-# inline empty #-}
  empty = nothing
  {-# inline (<|>) #-}
  (<|>) = (<!>)

instance Bind Maybe where
  {-# inline (>>-) #-}
  Maybe ma >>- f = Maybe (\n j -> ma n (\a -> unMaybe (f a) n j))

instance Monad Maybe where
  {-# inline (>>=) #-}
  (>>=) = (>>-)

instance MonadPlus Maybe where

instance MonadFix Maybe where
  {-# inline mfix #-}
  mfix f =
    let
      x = f (unMaybe x (errorWithoutStackTrace "mfix Maybe: Nothing") id)
    in
      x

instance MonadZip Maybe where
  {-# inline mzipWith #-}
  mzipWith = liftM2

instance Semigroup a => Semigroup (Maybe a) where
  {-# inline (<>) #-}
  Maybe ma <> Maybe mb = Maybe (\n j -> ma n (\a -> mb n (j . (a <>))))

instance Semigroup a => Monoid (Maybe a) where
  {-# inline mempty #-}
  mempty = nothing
  {-# inline mappend #-}
  mappend = (<>)

instance Foldable Maybe where
  {-# inline foldMap #-}
  foldMap f m = unMaybe m mempty f

instance Traversable Maybe where
  {-# inline traverse #-}
  traverse f m = unMaybe m (pure nothing) (fmap just . f)
