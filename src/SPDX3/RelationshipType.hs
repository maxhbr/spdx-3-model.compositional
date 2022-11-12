{-# LANGUAGE DeriveGeneric #-}
module SPDX3.RelationshipType where
import           Data.Aeson
import           GHC.Generics (Generic)

data RelationshipType
    = DESCRIBES
    | AMENDS
    | CONTAINS
    | ANCESTOR
    | DESCENDANT
    | VARIANT
    | DEPENDS_ON
    | BUILD_DEPENDENCY
    | DEV_DEPENDENCY
    | OPTIONAL_DEPENDENCY
    | PROVIDED_DEPENDENCY
    | TEST_DEPENDENCY
    | RUNTIME_DEPENDENCY
    | DEPENDENCY_MANIFEST
    | DYNAMIC_LINK
    | STATIC_LINK
    | PREREQUISITE

    | BUILD_TOOL
    | DEV_TOOL
    | TEST_TOOL

    | GENERATES

    | DISTRIBUTION_ARTIFACT
    | EXAMPLE
    | DATA_FILE
    | TEST_CASE
    | DOCUMENTATION
    | METAFILE
    | TEST
    | OPTIONAL_COMPONENT
    | PACKAGES

    | PATCH
    | COPY
    | EXPANDED_FROM_ARCHIVE
    | FILE_ADDED
    | FILE_DELETED
    | FILE_MODIFIED

    | SUPPLIED_BY
    | OTHER
    deriving (Generic, Eq, Show)
instance ToJSON RelationshipType where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON RelationshipType
