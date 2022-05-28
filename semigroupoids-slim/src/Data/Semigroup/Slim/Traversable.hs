{-|
Module      :  Data.Semigroup.Slim.Traversable
Copyright   :  (C) 2011-2015 Edward Kmett, (C) 2022 hololeap
Stability   :  provisional
Portability :  portable
License     :  BSD-style (see the file LICENSE)

Mostly copied directly from the @semigroupoids@ package

-}

{-# LANGUAGE Safe #-}

module Data.Semigroup.Slim.Traversable
  ( Traversable1(..)
  -- * Defining Traversable1 instances
  -- $traversable1instances
  , traverse1Maybe
  -- * Default superclass instance helpers
  , foldMap1Default
  ) where

import Control.Applicative
import Data.Semigroup.Slim.Traversable.Class
import Data.Functor.Slim.Bind.Class

-- | Default implementation of 'foldMap1' given an implementation of 'Traversable1'.
foldMap1Default :: (Traversable1 f, Semigroup m) => (a -> m) -> f a -> m
foldMap1Default f = getConst . traverse1 (Const . f)

-- $traversable1instances
-- Defining 'Traversable1' instances for types with both 'Traversable1' and 'Traversable'
-- substructures can be done with 'traverse1Maybe', '(<*.>)', and '(<.*>)'.
--
-- > data Foo a = Foo (Maybe a) (Maybe a) a [a]
-- >   deriving (Functor, Traversable, Foldable)
-- > instance Traversable1 Foo where
-- >   traverse1 f (Foo ma ma' a as) = Foo <$> traverseMaybe ma <*> traverseMaybe ma' <*.> f a <.*> traverseMaybe as
-- > instance Foldable1 Foo where
-- >   foldMap1 = foldMap1Default
