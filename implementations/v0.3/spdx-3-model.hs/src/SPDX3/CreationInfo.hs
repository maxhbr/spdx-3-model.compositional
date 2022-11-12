{-# LANGUAGE DeriveGeneric #-}
module SPDX3.CreationInfo
    where
import GHC.Generics (Generic)
import Data.Aeson
import SPDX3.RelationshipType

import SPDX3.SPDXID

type SemVer = String
type ProfileIdentifier = String
data DataLicense = CC0
    deriving (Generic, Eq, Show)
instance ToJSON DataLicense where
    toEncoding = genericToEncoding defaultOptions
data CreationInfo
    = CreationInfo
    { _specVer :: SemVer
    , _profile :: [ProfileIdentifier]
    , _created :: String
    , _dataLicense :: DataLicense
    , _createdBy :: [SPDXID]
    }
    deriving (Generic, Eq, Show)
instance ToJSON CreationInfo where
    toEncoding = genericToEncoding defaultOptions

mkCreationInfo :: CreationInfo
mkCreationInfo = CreationInfo "3.0.0" ["core"] "$UTC" CC0 []

class HasCreationInfo a where
    getCreationInfo :: a -> CreationInfo