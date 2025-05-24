{-|
Module      : Distribution.Portage.Types.DepSpec

Types for full Portage dependency specifications
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Distribution.Portage.Types.DepSpec
(   -- * Full dependency spec
      DepSpec(..)
    -- * Misc
    , Slot(..)
    , SubSlot(..)
    ) where

import Data.Data (Data)
import GHC.Generics (Generic)

import Data.Parsable

import Distribution.Portage.Types.Package
import Distribution.Portage.Types.UseDep
import Distribution.Portage.Types.VersionedPkg

data DepSpec =
    VersionedDepSpec
    { depSpecVersionedPkg :: VersionedPkg
    , depSpecUseDependency :: Maybe UseDependency
    , depSpecSlot :: Maybe Slot
    }
    | UnversionedDepSpec
    { depSpecPackage :: Package
    , depSpecUseDependency :: Maybe UseDependency
    , depSpecSlot :: Maybe Slot
    }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Parsable DepSpec st String where
    parserName = "portage full dependency spec"
    parser = choice
        [ try $ do
            vp <- parser
            ud <- optional parser
            s <- optional $ $( char ':' ) *> parser
            pure $ VersionedDepSpec vp ud s
        , do
            p <- parser
            ud <- optional parser
            s <- optional $ $( char ':' ) *> parser
            pure $ UnversionedDepSpec p ud s
        ]

instance Printable DepSpec where
    toString (VersionedDepSpec vp ud ms)
        =  toString vp
        ++ foldMap toString ud
        ++ foldMap (\s -> ":" ++ toString s) ms
    toString (UnversionedDepSpec p ud ms)
        =  toString p
        ++ foldMap toString ud
        ++ foldMap (\s -> ":" ++ toString s) ms

data Slot
    = AnySlot
    | AnySlotBreakable
    | SlotBreakable
        { unwrapSlot :: String
        , getSubSlot :: Maybe SubSlot
        }
    | Slot
        { unwrapSlot :: String
        , getSubSlot :: Maybe SubSlot
        }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable Slot where
    toString AnySlot = "*"
    toString AnySlotBreakable = "="
    toString (SlotBreakable s mss)
        =  s
        ++ foldMap (\ss -> "/" ++ toString ss) mss
        ++ "="
    toString (Slot s mss)
        =  s
        ++ foldMap (\ss -> "/" ++ toString ss) mss

instance Parsable Slot st e where
    parserName = "portage slot"
    parser = choice
        [ $( char '*' ) *> pure AnySlot
        , $( char '=' ) *> pure AnySlotBreakable
        , do
            s <- slotParser
            mss <- optional $ $(char '/') *> parser
            choice
                [ $( char '=' ) *> pure (SlotBreakable s mss)
                , pure (Slot s mss)
                ]
        ]

newtype SubSlot = SubSlot { unwrapSubSlot :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance Parsable SubSlot st e where
    parserName = "portage sub-slot"
    parser = SubSlot <$> slotParser

slotParser :: ParserT st e String
slotParser = wordAllowed wordStart wordRest
  where
    wordStart =
        [ isAsciiUpper
        , isAsciiLower
        , isDigit
        , (== '_')
        ]
    wordRest = wordStart ++
        [ (== '+')
        , (== '.')
        , (== '-')
        ]
