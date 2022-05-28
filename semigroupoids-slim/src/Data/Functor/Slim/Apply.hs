{-|
Module      :  Data.Functor.Slim.Apply
Copyright   :  (C) 2011-2015 Edward Kmett, (C) 2022 hololeap
Stability   :  provisional
Portability :  portable
License     :  BSD-style (see the file LICENSE)

Mostly copied directly from the @semigroupoids@ package

-}

{-# Language DeriveTraversable #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language Trustworthy #-}

module Data.Functor.Slim.Apply
  ( Apply(..)
  , WrappedApplicative(..)
  , MaybeApply(..)
  , (<.*>)
  , (<*.>)
  , traverse1Maybe
  , (<..>)
  , liftF3
  , Ap'(..)
  ) where

import Data.Functor.Slim.Bind.Class

infixl 4 <..>

-- | A variant of '<.>' with the arguments reversed.
(<..>) :: Apply w => w a -> w (a -> b) -> w b
(<..>) = liftF2 (flip id)
{-# INLINE (<..>) #-}

-- | Lift a ternary function into a comonad with zipping
liftF3 :: Apply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftF3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftF3 #-}

-- | This data type witnesses the lifting of a 'Semigroup' into an 'Apply' pointwise.
newtype Ap' f a = Ap' { getAp' :: f a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Apply)

instance (Apply f, Semigroup a) => Semigroup (Ap' f a) where
  (<>) = liftF2 (<>)
