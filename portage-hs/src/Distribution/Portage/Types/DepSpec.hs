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

module Distribution.Portage.Types.DepSpec
(   -- * Full dependency spec
      DepSpec(..)
    -- * Misc
    , Slot(..)
    , SubSlot(..)
    , Repository(..)
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
    , depSpecRepository :: Maybe Repository
    }
    | UnversionedDepSpec
    { depSpecPackage :: Package
    , depSpecUseDependency :: Maybe UseDependency
    , depSpecSlot :: Maybe Slot
    , depSpecRepository :: Maybe Repository
    }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Stream s m Char => Parsable DepSpec m u s where
    parserName = "portage full dependency spec"
    parser = choice
        [ do
            vp <- parser
            ud <- optionMaybe parser
            s <- optionMaybe $ try (char ':' *> parser)
            r <- optionMaybe $ string "::" *> parser
            pure $ VersionedDepSpec vp ud s r
        , do
            p <- parser
            ud <- optionMaybe parser
            s <- optionMaybe $ try (char ':' *> parser)
            r <- optionMaybe $ string "::" *> parser
            pure $ UnversionedDepSpec p ud s r
        ]

instance Printable DepSpec where
    toString (VersionedDepSpec vp ud ms mr)
        =  toString vp
        ++ foldMap toString ud
        ++ foldMap (\s -> ":" ++ toString s) ms
        ++ foldMap (\r -> "::" ++ toString r) mr
    toString (UnversionedDepSpec p ud ms mr)
        =  toString p
        ++ foldMap toString ud
        ++ foldMap (\s -> ":" ++ toString s) ms
        ++ foldMap (\r -> "::" ++ toString r) mr

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

instance Stream s m Char => Parsable Slot m u s where
    parserName = "portage slot"
    parser = choice
        [ char '*' *> pure AnySlot
        , char '=' *> pure AnySlotBreakable
        , do
            s <- slotParser
            mss <- optionMaybe $ char '/' *> parser
            choice
                [ char '=' *> pure (SlotBreakable s mss)
                , pure (Slot s mss)
                ]
        ]

newtype SubSlot = SubSlot { unwrapSubSlot :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance Stream s m Char => Parsable SubSlot m u s where
    parserName = "portage sub-slot"
    parser = SubSlot <$> slotParser

slotParser :: Stream s m Char => ParsecT s u m String
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

newtype Repository = Repository { unwrapRepository :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance Stream s m Char => Parsable Repository m u s where
    parserName = "portage repository"
    parser = Repository <$> pkgParser wordStart wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]

        wordRest = (== '-') : wordStart
