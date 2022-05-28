{-|
Module      : Data.Functor.Plus
Copyright   :  (C) 2011-2015 Edward Kmett, (C) 2022 hololeap
Stability   :  provisional
Portability :  portable
License     :  BSD-style (see the file LICENSE)

Mostly copied directly from the @semigroupoids@ package
-}

{-# LANGUAGE Safe #-}

module Data.Functor.Slim.Plus
  ( Plus(..)
  , psum
  , module Data.Functor.Slim.Alt
  ) where

import Control.Applicative hiding (some, many)
import Control.Arrow
import Control.Monad
import Data.Functor.Slim.Apply
import Data.Functor.Slim.Alt
import Data.Functor.Compose
import Data.Functor.Product
import qualified Data.Monoid as Monoid
import Data.Proxy

-- | Laws:
--
-- > zero <!> m = m
-- > m <!> zero = m
--
-- If extended to an 'Alternative' then 'zero' should equal 'empty'.
class Alt f => Plus f where
  zero :: f a

-- | The sum of a collection of actions, generalizing 'concat'.
--
-- >>> psum [Just "Hello", Nothing, Just "World"]
-- Just "Hello"
--
-- @since 5.3.6
psum :: (Foldable t, Plus f) => t (f a) -> f a
psum = foldr (<!>) zero

instance Plus Proxy where
  zero = Proxy

instance Plus IO where
  zero = error "zero"

instance Plus [] where
  zero = []

instance Plus Maybe where
  zero = Nothing

instance MonadPlus m => Plus (WrappedMonad m) where
  zero = empty

instance ArrowPlus a => Plus (WrappedArrow a b) where
  zero = empty

instance Alternative f => Plus (WrappedApplicative f) where
  zero = empty

instance (Plus f, Functor g) => Plus (Compose f g) where
  zero = Compose zero

instance (Plus f, Plus g) => Plus (Product f g) where
  zero = Pair zero zero

instance Plus Monoid.First where
  zero = Monoid.First Nothing

instance Plus Monoid.Last where
  zero = Monoid.Last Nothing
