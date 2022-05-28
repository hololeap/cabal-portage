{-|
Module      :  Data.Semigroup.Slim.Foldable.Class
Copyright   :  (C) 2011-2015 Edward Kmett, (C) 2022 hololeap
Stability   :  provisional
Portability :  portable
License     :  BSD-style (see the file LICENSE)

Mostly copied directly from the @semigroupoids@ package

-}

{-# LANGUAGE Safe #-}

module Data.Semigroup.Slim.Foldable.Class
  ( Foldable1(..)
  , Bifoldable1(..)
  ) where

import Control.Applicative
import Data.Bifoldable
import Data.Foldable

import Data.Functor.Identity
import Data.Functor.Product as Functor
import Data.Functor.Sum as Functor
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty(..))
import Data.Complex

import qualified Data.Monoid as Monoid
import Data.Semigroup as Semigroup hiding (Product, Sum)

import Prelude hiding (foldr)

class Foldable t => Foldable1 t where
  fold1 :: Semigroup m => t m -> m
  foldMap1 :: Semigroup m => (a -> m) -> t a -> m
  toNonEmpty :: t a -> NonEmpty a

  foldMap1 f = maybe (error "foldMap1") id . getOptionCompat . foldMap (optionCompat . Just . f)
  fold1 = foldMap1 id
  toNonEmpty = foldMap1 (:|[])

instance Foldable1 Monoid.Sum where
  foldMap1 f (Monoid.Sum a) = f a

instance Foldable1 Monoid.Product where
  foldMap1 f (Monoid.Product a) = f a

instance Foldable1 Monoid.Dual where
  foldMap1 f (Monoid.Dual a) = f a

instance Foldable1 f => Foldable1 (Monoid.Alt f) where
  foldMap1 g (Monoid.Alt m) = foldMap1 g m

instance Foldable1 Semigroup.First where
  foldMap1 f (Semigroup.First a) = f a

instance Foldable1 Semigroup.Last where
  foldMap1 f (Semigroup.Last a) = f a

instance Foldable1 Semigroup.Min where
  foldMap1 f (Semigroup.Min a) = f a

instance Foldable1 Semigroup.Max where
  foldMap1 f (Semigroup.Max a) = f a

class Bifoldable t => Bifoldable1 t where
  bifold1 :: Semigroup m => t m m -> m
  bifold1 = bifoldMap1 id id
  {-# INLINE bifold1 #-}

  bifoldMap1 :: Semigroup m => (a -> m) -> (b -> m) -> t a b -> m
  bifoldMap1 f g = maybe (error "bifoldMap1") id
                 . getOptionCompat
                 . bifoldMap (optionCompat . Just . f) (optionCompat . Just . g)
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 Arg where
  bifoldMap1 f g (Arg a b) = f a <> g b

instance Bifoldable1 Either where
  bifoldMap1 f _ (Left a) = f a
  bifoldMap1 _ g (Right b) = g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 (,) where
  bifoldMap1 f g (a, b) = f a <> g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,) x) where
  bifoldMap1 f g (_,a,b) = f a <> g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,,) x y) where
  bifoldMap1 f g (_,_,a,b) = f a <> g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,,,) x y z) where
  bifoldMap1 f g (_,_,_,a,b) = f a <> g b
  {-# INLINE bifoldMap1 #-}

instance Bifoldable1 Const where
  bifoldMap1 f _ (Const a) = f a
  {-# INLINE bifoldMap1 #-}



instance Foldable1 Complex where
  foldMap1 f (a :+ b) = f a <> f b
  {-# INLINE foldMap1 #-}

instance Foldable1 Identity where
  foldMap1 f = f . runIdentity

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
  foldMap1 f = foldMap1 (foldMap1 f) . getCompose

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Product f g) where
  foldMap1 f (Functor.Pair a b) = foldMap1 f a <> foldMap1 f b

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Sum f g) where
  foldMap1 f (Functor.InL x) = foldMap1 f x
  foldMap1 f (Functor.InR y) = foldMap1 f y

instance Foldable1 NonEmpty where
  foldMap1 f (a :| as) = foldr (\b g x -> f x <> g b) f as a
  toNonEmpty = id

instance Foldable1 ((,) a) where
  foldMap1 f (_, x) = f x

-- The default implementations of foldMap1 and bifoldMap1 above require the use
-- of a Maybe type with the following Monoid instance:
--
--   instance Semigroup a => Monoid (Maybe a) where ...
--
-- Unfortunately, Maybe has only had such an instance since base-4.11. Prior
-- to that, its Monoid instance had an instance context of Monoid a, which is
-- too strong. To compensate, we use CPP to define an OptionCompat type
-- synonym, which is an alias for Maybe on recent versions of base and an alias
-- for Data.Semigroup.Option on older versions of base. We don't want to use
-- Option on recent versions of base, as it has been removed.
type OptionCompat = Maybe

optionCompat :: Maybe a -> OptionCompat a
optionCompat = id

getOptionCompat :: OptionCompat a -> Maybe a
getOptionCompat = id
