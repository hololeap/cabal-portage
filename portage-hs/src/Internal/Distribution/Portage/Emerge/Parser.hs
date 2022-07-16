{-# Language ApplicativeDo #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}

module Internal.Distribution.Portage.Emerge.Parser
    ( emergeParser
    ) where

import Control.Monad.Combinators
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import Data.Void

import Data.Parsable hiding (space)
import Internal.Distribution.Portage.Types


emergeParser :: Parsec Void Text (Set Package)
emergeParser
    = fmap (S.fromList . catMaybes) (sepEndBy lineParser newline)

lineParser :: Parsec Void Text (Maybe Package)
lineParser = Just <$> try packageLine <|> Nothing <$ skipRest

packageLine :: Parsec Void Text Package
packageLine = do
    _ <- space
    _ <- single '['
    _ <- takeWhileP Nothing (\c -> c /= ']' && c /= '\n')
    _ <- single ']'
    _ <- space
    pkg <- parser
    _ <- space
    _ <- skipRest
    pure pkg

skipRest :: Parsec Void Text ()
skipRest = void $ takeWhileP Nothing (/= '\n')

space :: Parsec Void Text ()
space = void $ takeWhileP Nothing (\c -> c == ' ' || c == '\t')
