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
    , Repository(..)
    -- ** Versions
    , Version(..)
    , VersionNum(..)
    , VersionLetter(..)
    , VersionSuffix(..)
    , VersionSuffixNum(..)
    , VersionRevision(..)
    ) where

import Distribution.Portage.Types.Internal


