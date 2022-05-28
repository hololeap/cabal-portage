{-|
Module      :  Data.Semigroup.Slim.Traversable.Class
Copyright   :  (C) 2011-2015 Edward Kmett, (C) 2022 hololeap
Stability   :  provisional
Portability :  portable
License     :  BSD-style (see the file LICENSE)

Mostly copied directly from the @semigroupoids@ package

-}

{-# LANGUAGE Safe #-}

module Data.Semigroup.Slim.Traversable.Class
  ( Bitraversable1(..)
  , Traversable1(..)
  ) where

import Control.Applicative
import Data.Bitraversable
import Data.Bifunctor
import Data.Functor.Slim.Apply
import Data.Functor.Compose

import Data.Functor.Product as Functor
import Data.Functor.Sum as Functor
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import Data.Semigroup as Semigroup
import Data.Semigroup.Slim.Foldable.Class
import Data.Complex

class (Bifoldable1 t, Bitraversable t) => Bitraversable1 t where
  bitraverse1 :: Apply f => (a -> f b) -> (c -> f d) -> t a c -> f (t b d)
  bitraverse1 f g  = bisequence1 . bimap f g
  {-# INLINE bitraverse1 #-}

  bisequence1 :: Apply f => t (f a) (f b) -> f (t a b)
  bisequence1 = bitraverse1 id id
  {-# INLINE bisequence1 #-}

  {-# MINIMAL bitraverse1 | bisequence1 #-}

instance Bitraversable1 Arg where
  bitraverse1 f g (Arg a b) = Arg <$> f a <.> g b

instance Bitraversable1 Either where
  bitraverse1 f _ (Left a) = Left <$> f a
  bitraverse1 _ g (Right b) = Right <$> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 (,) where
  bitraverse1 f g (a, b) = (,) <$> f a <.> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 ((,,) x) where
  bitraverse1 f g (x, a, b) = (,,) x <$> f a <.> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 ((,,,) x y) where
  bitraverse1 f g (x, y, a, b) = (,,,) x y <$> f a <.> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 ((,,,,) x y z) where
  bitraverse1 f g (x, y, z, a, b) = (,,,,) x y z <$> f a <.> g b
  {-# INLINE bitraverse1 #-}

instance Bitraversable1 Const where
  bitraverse1 f _ (Const a) = Const <$> f a
  {-# INLINE bitraverse1 #-}

class (Foldable1 t, Traversable t) => Traversable1 t where
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)
  sequence1 :: Apply f => t (f b) -> f (t b)

  sequence1 = traverse1 id
  traverse1 f = sequence1 . fmap f

  {-# MINIMAL traverse1 | sequence1 #-}

instance (Traversable1 f, Traversable1 g) => Traversable1 (Compose f g) where
  traverse1 f = fmap Compose . traverse1 (traverse1 f) . getCompose

instance (Traversable1 f, Traversable1 g) => Traversable1 (Functor.Product f g) where
  traverse1 f (Functor.Pair a b) = Functor.Pair <$> traverse1 f a <.> traverse1 f b

instance (Traversable1 f, Traversable1 g) => Traversable1 (Functor.Sum f g) where
  traverse1 f (Functor.InL x) = Functor.InL <$> traverse1 f x
  traverse1 f (Functor.InR y) = Functor.InR <$> traverse1 f y


instance Traversable1 Complex where
  traverse1 f (a :+ b) = (:+) <$> f a <.> f b
  {-# INLINE traverse1 #-}

instance Traversable1 NonEmpty where
  traverse1 f (a :| as) = foldr (\b g x -> (\a' (b':| bs') -> a' :| b': bs') <$> f x <.> g b) (fmap (:|[]) . f) as a

instance Traversable1 ((,) a) where
  traverse1 f (a, b) = (,) a <$> f b

instance Traversable1 Monoid.Sum where
  traverse1 g (Monoid.Sum a) = Monoid.Sum <$> g a

instance Traversable1 Monoid.Product where
  traverse1 g (Monoid.Product a) = Monoid.Product <$> g a

instance Traversable1 Monoid.Dual where
  traverse1 g (Monoid.Dual a) = Monoid.Dual <$> g a

instance Traversable1 f => Traversable1 (Monoid.Alt f) where
  traverse1 g (Monoid.Alt m) = Monoid.Alt <$> traverse1 g m

instance Traversable1 Semigroup.First where
  traverse1 g (Semigroup.First a) = Semigroup.First <$> g a

instance Traversable1 Semigroup.Last where
  traverse1 g (Semigroup.Last a) = Semigroup.Last <$> g a

instance Traversable1 Semigroup.Min where
  traverse1 g (Semigroup.Min a) = Semigroup.Min <$> g a

instance Traversable1 Semigroup.Max where
  traverse1 g (Semigroup.Max a) = Semigroup.Max <$> g a
