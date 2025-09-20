{-# Language DeriveAnyClass #-}
{-# Language DerivingVia #-}

{-# Options_GHC -Wno-orphans #-}

module Distribution.Portage.Types.Orphans where

import Data.Hashable

import Distribution.Portage.Types

deriving newtype instance Hashable Category
deriving newtype instance Hashable PkgName
deriving anyclass instance Hashable Package
deriving newtype instance Hashable VersionNum
deriving newtype instance Hashable VersionLetter
deriving anyclass instance Hashable VersionSuffix
deriving newtype instance Hashable VersionSuffixNum
deriving newtype instance Hashable VersionRevision
deriving anyclass instance Hashable Version
deriving anyclass instance Hashable Slot
deriving newtype instance Hashable SubSlot
deriving newtype instance Hashable Repository
