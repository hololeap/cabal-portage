{-|
Module      : Distribution.Portage.Cabal.Error

Error types for cabal-portage
-}

{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}

module Distribution.Portage.Cabal.Error
    ( CabalPortageError(..)
    , parseCabalPortage
    ) where

import Data.Bifunctor
import Control.Monad.Except
import Data.Parsable

data CabalPortageError =
    PortageParseError String ParseError

parseCabalPortage
    :: (MonadError CabalPortageError m, Parsable t, ParsableInput t ~ ())
    => (ParseError -> CabalPortageError)
    -> SourceName
    -> String
    -> m (PartialParse t)
parseCabalPortage c n = liftEither . first c . runParsable n
