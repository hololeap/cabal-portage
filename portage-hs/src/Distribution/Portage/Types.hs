{-|
Module      : Distribution.Portage.Types

Types for Portage atoms, etc.
-}

{-# Language NoImplicitPrelude #-}

module Distribution.Portage.Types
    (
    -- * Types
      Package(..)
    , Category(..)
    , PkgName(..)
    , Slot(..)
    , SubSlot(..)
    , Repository(..)
    -- ** Versions
    , Version(..)
    , VersionNum(..)
    , VersionLetter(..)
    , VersionSuffix(..)
    , VersionSuffixNum(..)
    , VersionRevision(..)
    -- ** Constraints
    , ConstrainedDep(..)
    , toConstrainedDep
    , fromConstrainedDep
    , doesConstraintMatch
    , Operator(..)
    -- * Internal
    , FauxVersion(..)
    , FauxVersionNum(..)
    ) where

import Internal.Distribution.Portage.Types


