{-# Language ApplicativeDo #-}
{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}

module Distribution.Portage.Emerge.Parser
    ( emergeParser
    ) where

import Control.Applicative (some)
-- import Control.Monad.Combinators
import Data.Coerce
import Data.Monoid (Last(..))
-- import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Set (Set)
-- import Text.Parsec.Language (haskell)
-- import Text.Parsec.Token (GenTokenParser(decimal))
import Text.Read (readMaybe)

import Data.Parsable hiding (space)
import Distribution.Portage.Types

-- | Returns a set of packages and the count as reported by the emerge output
emergeParser :: (Stream s m Char, Parsable Package m u s)
    => ParsecT s u m (Integer, Set Package)
emergeParser = do
    r <- sepEndBy lineParser newline
    case coerce $ foldMap go r of
        (Nothing, _) -> fail e
        (Just i , s) -> pure (i, s)
  where
    go :: EmergeLine -> (Last Integer, Set Package)
    go = coerce . \case
        PackageLine p -> (Nothing, S.singleton p)
        TotalLine   i -> (Just i , S.empty      )
        OtherLine     -> (Nothing, S.empty      )

    e :: String
    e = "emerge output contains a package total"

data EmergeLine
    = PackageLine Package
    | TotalLine Integer
    | OtherLine
    deriving (Show, Eq, Ord)


lineParser :: (Stream s m Char, Parsable Package m u s)
    => ParsecT s u m EmergeLine
lineParser = choice
    [ PackageLine <$> try packageLine
    , TotalLine   <$> try totalLine
    , OtherLine   <$  skipRest
    ]

packageLine :: (Stream s m Char, Parsable Package m u s) => ParsecT s u m Package
packageLine = do
    _ <- space
    _ <- char '['
    _ <- many $ satisfy (\c -> c /= ']' && c /= '\n')
    _ <- char ']'
    _ <- space
    pkg <- parser
    _ <- space
    _ <- skipRest
    pure pkg

totalLine :: Stream s m Char => ParsecT s u m Integer
totalLine = do
    _ <- string "Total:"
    _ <- space
    ds <- some digit
    _ <- space
    _ <- string "packages"
    _ <- skipRest
    case readMaybe ds of
        Just n  -> pure n
        Nothing -> fail $ "Could not parse as Integer: " ++ show ds

skipRest :: Stream s m Char => ParsecT s u m ()
skipRest = void $ many $ satisfy (/= '\n')

space :: Stream s m Char => ParsecT s u m ()
space = void $ many $ satisfy (\c -> c == ' ' || c == '\t')
