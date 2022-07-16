{-# Language ApplicativeDo #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}

module Internal.Distribution.Portage.Emerge.Parser
    ( emergeParser
    ) where

import Control.Monad.Combinators
import qualified Data.Set as S
import Data.Set (Set)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Parsable
import Internal.Distribution.Portage.Types

emergeParser :: forall e s m. (Parsable Package m s e, Token s ~ Char)
    => ParseResult e s m (Set Package)
emergeParser = choice
    [ do
        this <- try packageLine <|> (S.empty <$ skipLine)
        rest <- emergeParser
        pure $ this <> rest
    , checkCoverage $ S.empty <$ eof
    ]
  where
    packageLine :: ParseResult e s m (Set Package)
    packageLine = do
        _ <- space
        _ <- single '['
        _ <- takeWhileP Nothing (/= ']')
        _ <- single ']'
        _ <- space
        pkg <- parser
        _ <- space
        _ <- skipLine
        pure $ S.singleton pkg

    skipLine :: ParseResult e s m ()
    skipLine = do
        _ <- takeWhileP Nothing (\c -> c /= '\r' && c /= '\n')
        _ <- newline
        pure ()
