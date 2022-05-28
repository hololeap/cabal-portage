{-|
Module      :  Data.Functor.Slim.Alt
Copyright   :  (C) 2011-2015 Edward Kmett, (C) 2022 hololeap
Stability   :  provisional
Portability :  portable
License     :  BSD-style (see the file LICENSE)

Mostly copied directly from the @semigroupoids@ package
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ConstrainedClassMethods #-}

{-# options_ghc -fno-warn-deprecations #-}

module Data.Functor.Slim.Alt
  ( Alt(..)
  , optional
  , module Data.Functor.Slim.Apply
  ) where

import Control.Applicative hiding (some, many, optional)
import Control.Arrow
import Control.Exception (catch, SomeException)
import Control.Monad

import Data.Functor.Slim.Apply
import Data.Functor.Compose
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Product
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Data.Proxy

infixl 3 <!>

-- | Laws:
--
-- > <!> is associative:             (a <!> b) <!> c = a <!> (b <!> c)
-- > <$> left-distributes over <!>:  f <$> (a <!> b) = (f <$> a) <!> (f <$> b)
--
-- If extended to an 'Alternative' then '<!>' should equal '<|>'.
--
-- Ideally, an instance of 'Alt' also satisfies the \"left distribution\" law of
-- MonadPlus with respect to '<.>':
--
-- > <.> right-distributes over <!>: (a <!> b) <.> c = (a <.> c) <!> (b <.> c)
--
-- 'IO', @'Either' a@, @'ExceptT' e m@ and 'GHC.Conc.STM' instead satisfy the
-- \"left catch\" law:
--
-- > pure a <!> b = pure a
--
-- 'Maybe' and 'Identity' satisfy both \"left distribution\" and \"left catch\".
--
-- These variations cannot be stated purely in terms of the dependencies of 'Alt'.
--
-- When and if MonadPlus is successfully refactored, this class should also
-- be refactored to remove these instances.
--
-- The right distributive law should extend in the cases where the a 'Bind' or 'Monad' is
-- provided to yield variations of the right distributive law:
--
-- > (m <!> n) >>- f = (m >>- f) <!> (m >>- f)
-- > (m <!> n) >>= f = (m >>= f) <!> (m >>= f)

class Functor f => Alt f where
  -- | '<|>' without a required @empty@
  (<!>) :: f a -> f a -> f a

  some :: Applicative f => f a -> f [a]
  some v = some_v
    where many_v = some_v <!> pure []
          some_v = (:) <$> v <*> many_v

  many :: Applicative f => f a -> f [a]
  many v = many_v
    where many_v = some_v <!> pure []
          some_v = (:) <$> v <*> many_v

-- | One or none.
optional :: (Alt f, Applicative f) => f a -> f (Maybe a)
optional v = Just <$> v <!> pure Nothing

instance Alt Proxy where
  _ <!> _ = Proxy
  some _ = Proxy
  many _ = Proxy

instance Alt (Either a) where
  Left _ <!> b = b
  a      <!> _ = a

-- | This instance does not actually satisfy the ('<.>') right distributive law
-- It instead satisfies the \"left catch\" law
instance Alt IO where
  m <!> n = catch m (go n) where
    go :: x -> SomeException -> x
    go = const

-- | Choose the first option every time. While \'choose the last option\' every
-- time is also valid, this instance satisfies more laws.
--
-- @since 5.3.6
instance Alt Identity where
  {-# INLINEABLE (<!>) #-}
  m <!> _ = m
  some (Identity x) = Identity . repeat $ x
  many (Identity x) = Identity . repeat $ x

instance Alt [] where
  (<!>) = (++)

instance Alt Maybe where
  Nothing <!> b = b
  a       <!> _ = a

instance MonadPlus m => Alt (WrappedMonad m) where
  (<!>) = (<|>)

instance ArrowPlus a => Alt (WrappedArrow a b) where
  (<!>) = (<|>)

instance Alt NonEmpty where
  (a :| as) <!> ~(b :| bs) = a :| (as ++ b : bs)

instance Alternative f => Alt (WrappedApplicative f) where
  WrapApplicative a <!> WrapApplicative b = WrapApplicative (a <|> b)

instance (Alt f, Functor g) => Alt (Compose f g) where
  Compose a <!> Compose b = Compose (a <!> b)

instance (Alt f, Alt g) => Alt (Product f g) where
  Pair a1 b1 <!> Pair a2 b2 = Pair (a1 <!> a2) (b1 <!> b2)

instance Alt Semigroup.First where
  (<!>) = (<>)

instance Alt Semigroup.Last where
  (<!>) = (<>)

instance Alt Monoid.First where
  (<!>) = mappend

instance Alt Monoid.Last where
  (<!>) = mappend
