{-|
Module      :  Data.Functor.Bind.Class
Copyright   :  (C) 2011-2015 Edward Kmett, (C) 2022 hololeap
Stability   :  provisional
Portability :  portable
License     :  BSD-style (see the file LICENSE)

Mostly copied directly from the @semigroupoids@ package
-}

{-# Language Safe #-}

module Data.Functor.Slim.Bind.Class
  (
  -- * Apply
    Apply(..)
  , WrappedApplicative(..)
  , MaybeApply(..)
  , (<.*>)
  , (<*.>)
  , traverse1Maybe
  -- * Bind
  , Bind(..)
  , returning
  , apDefault
  ) where

import Control.Applicative
import Data.Functor
import Data.Functor.Compose

infixl 1 >>-
infixl 4 <.>, <., .>

-- | A strong lax semi-monoidal endofunctor.
-- This is equivalent to an 'Applicative' without 'pure'.
--
-- Laws:
--
-- @
-- ('.') '<$>' u '<.>' v '<.>' w = u '<.>' (v '<.>' w)
-- x '<.>' (f '<$>' y) = ('.' f) '<$>' x '<.>' y
-- f '<$>' (x '<.>' y) = (f '.') '<$>' x '<.>' y
-- @
--
-- The laws imply that `.>` and `<.` really ignore their
-- left and right results, respectively, and really
-- return their right and left results, respectively.
-- Specifically,
--
-- @
-- (mf '<$>' m) '.>' (nf '<$>' n) = nf '<$>' (m '.>' n)
-- (mf '<$>' m) '<.' (nf '<$>' n) = mf '<$>' (m '<.' n)
-- @
class Functor f => Apply f where
  (<.>) :: f (a -> b) -> f a -> f b
  (<.>) = liftF2 id

  -- | @ a '.>' b = 'const' 'id' '<$>' a '<.>' b @
  (.>) :: f a -> f b -> f b
  a .> b = const id <$> a <.> b

  -- | @ a '<.' b = 'const' '<$>' a '<.>' b @
  (<.) :: f a -> f b -> f a
  a <. b = const <$> a <.> b

  -- | Lift a binary function into a comonad with zipping
  liftF2 :: (a -> b -> c) -> f a -> f b -> f c
  liftF2 f a b = f <$> a <.> b
  {-# INLINE liftF2 #-}

  {-# MINIMAL (<.>) | liftF2 #-}



instance (Apply f, Apply g) => Apply (Compose f g) where
  Compose f <.> Compose x = Compose ((<.>) <$> f <.> x)

-- | A @'Const' m@ is not 'Applicative' unless its @m@ is a 'Monoid', but it is an instance of 'Apply'
instance Semigroup m => Apply (Const m) where
  Const m <.> Const n = Const (m <> n)
  Const m <.  Const n = Const (m <> n)
  Const m  .> Const n = Const (m <> n)

-- | A 'Monad' sans 'return'.
--
-- Minimal definition: Either 'join' or '>>-'
--
-- If defining both, then the following laws (the default definitions) must hold:
--
-- > join = (>>- id)
-- > m >>- f = join (fmap f m)
--
-- Laws:
--
-- > induced definition of <.>: f <.> x = f >>- (<$> x)
--
-- Finally, there are two associativity conditions:
--
-- > associativity of (>>-):    (m >>- f) >>- g == m >>- (\x -> f x >>- g)
-- > associativity of join:     join . join = join . fmap join
--
-- These can both be seen as special cases of the constraint that
--
-- > associativity of (->-): (f ->- g) ->- h = f ->- (g ->- h)
--

class Apply m => Bind m where
  (>>-) :: m a -> (a -> m b) -> m b
  m >>- f = join (fmap f m)

  join :: m (m a) -> m a
  join = (>>- id)

  {-# MINIMAL (>>-) | join #-}

returning :: Functor f => f a -> (a -> b) -> f b
returning = flip fmap

apDefault :: Bind f => f (a -> b) -> f a -> f b
apDefault f x = f >>- \f' -> f' <$> x

-- | Wrap an 'Applicative' to be used as a member of 'Apply'
newtype WrappedApplicative f a = WrapApplicative { unwrapApplicative :: f a }

instance Functor f => Functor (WrappedApplicative f) where
  fmap f (WrapApplicative a) = WrapApplicative (f <$> a)

instance Applicative f => Apply (WrappedApplicative f) where
  WrapApplicative f <.> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <.  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  .> WrapApplicative b = WrapApplicative (a  *> b)

instance Applicative f => Applicative (WrappedApplicative f) where
  pure = WrapApplicative . pure
  WrapApplicative f <*> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <*  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  *> WrapApplicative b = WrapApplicative (a  *> b)

instance Alternative f => Alternative (WrappedApplicative f) where
  empty = WrapApplicative empty
  WrapApplicative a <|> WrapApplicative b = WrapApplicative (a <|> b)

-- | Transform an Apply into an Applicative by adding a unit.
newtype MaybeApply f a = MaybeApply { runMaybeApply :: Either (f a) a }

-- | Apply a non-empty container of functions to a possibly-empty-with-unit container of values.
(<.*>) :: (Apply f) => f (a -> b) -> MaybeApply f a -> f b
ff <.*> MaybeApply (Left fa) = ff <.> fa
ff <.*> MaybeApply (Right a) = ($ a) <$> ff
infixl 4 <.*>

-- | Apply a possibly-empty-with-unit container of functions to a non-empty container of values.
(<*.>) :: (Apply f) => MaybeApply f (a -> b) -> f a -> f b
MaybeApply (Left ff) <*.> fa = ff <.> fa
MaybeApply (Right f) <*.> fa = f <$> fa
infixl 4 <*.>

-- | Traverse a 'Traversable' using 'Apply', getting the results back in a 'MaybeApply'.
traverse1Maybe :: (Traversable t, Apply f) => (a -> f b) -> t a -> MaybeApply f (t b)
traverse1Maybe f = traverse (MaybeApply . Left . f)

instance Functor f => Functor (MaybeApply f) where
  fmap f (MaybeApply (Right a)) = MaybeApply (Right (f     a ))
  fmap f (MaybeApply (Left fa)) = MaybeApply (Left  (f <$> fa))

instance Apply f => Apply (MaybeApply f) where
  MaybeApply (Right f) <.> MaybeApply (Right a) = MaybeApply (Right (f         a ))
  MaybeApply (Right f) <.> MaybeApply (Left fa) = MaybeApply (Left  (f     <$> fa))
  MaybeApply (Left ff) <.> MaybeApply (Right a) = MaybeApply (Left  (($ a) <$> ff))
  MaybeApply (Left ff) <.> MaybeApply (Left fa) = MaybeApply (Left  (ff    <.> fa))

  MaybeApply a         <. MaybeApply (Right _) = MaybeApply a
  MaybeApply (Right a) <. MaybeApply (Left fb) = MaybeApply (Left (a  <$ fb))
  MaybeApply (Left fa) <. MaybeApply (Left fb) = MaybeApply (Left (fa <. fb))

  MaybeApply (Right _) .> MaybeApply b = MaybeApply b
  MaybeApply (Left fa) .> MaybeApply (Right b) = MaybeApply (Left (fa $> b ))
  MaybeApply (Left fa) .> MaybeApply (Left fb) = MaybeApply (Left (fa .> fb))

instance Apply f => Applicative (MaybeApply f) where
  pure a = MaybeApply (Right a)
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

