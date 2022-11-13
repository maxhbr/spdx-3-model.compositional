{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module SPDX3.From2
    where
import           Control.Monad.Reader
import           Data.Aeson
import Data.Maybe (fromMaybe)
import           Data.Aeson.Types
import qualified Data.HashMap.Strict    as Map
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           GHC.Word               (Word8)
import SPDX3.Model.Common
import           SPDX3.Model.CreationInfo
import           SPDX3.Model.ExternalIdentifier
import           SPDX3.Model.ExternalReference
import           SPDX3.Model.IntegrityMethod
import           SPDX3.Model.RelationshipType
import           SPDX3.Model.SPDXID
import           SPDX3.Model
import           SPDX3.Monad
import           Data.Time.Clock
import qualified Data.ByteString.Lazy as BSL
import           Data.Time.Format.ISO8601

import qualified SPDX.Document as SPDX2
import qualified SPDX.Document.RelationshipTypes as SPDX2R

convertCreationInfo :: SPDX2.SPDXCreationInfo -> String -> (CreationInfo, Maybe String)
convertCreationInfo ci spdx2DataLicense = let
      spdx2CreationInfoComment = SPDX2._SPDXCreationInfo_comment ci
      spdx2CreationInfoCreated = SPDX2._SPDXCreationInfo_created ci
      spdx2CreationInfoCreators = SPDX2._SPDXCreationInfo_creators ci
      spdx2CreationInfoLicenseListVersion = SPDX2._SPDXCreationInfo_licenseListVersion ci

      parseSpdx2Creator :: String -> Actor
      parseSpdx2Creator ('P':'e':'r':'s':'o':'n':':':' ':name) = Actor (Just name) (Just PERSON)
      parseSpdx2Creator ('O':'r':'g':'a':'n':'i':'z':'a':'t':'i':'o':'n':':':' ':name) = Actor (Just name) (Just ORGANIZATION)
      parseSpdx2Creator ('T':'o':'o':'l':':':' ':name) = Actor (Just name) (Just TOOL)
      parseSpdx2Creator name = Actor (Just name) Nothing

      spdx3CreationInfoCreated = let
            fallback = fromMaybe undefined (iso8601ParseM "2022-11-13T13:14:36.324980945Z")
        in fromMaybe fallback (iso8601ParseM spdx2CreationInfoCreated)
    in ((CreationInfo
            { _specVer = "3.0.0"
            , _profile = ["core", "software", "licensing"]
            , _created = spdx3CreationInfoCreated
            , _dataLicense = case spdx2DataLicense of
                "CC0-1.0" -> CC0
                _ -> undefined
            , _createdBy = map parseSpdx2Creator spdx2CreationInfoCreators
            }), spdx2CreationInfoLicenseListVersion)

convertFile :: CreationInfo -> SPDX2.SPDXFile -> SPDX File
convertFile ci file = let
        spdx2FileSPDXID = SPDX2._SPDXFile_SPDXID file
        spdx2FileFileName = SPDX2._SPDXFile_fileName file
        spdx2FileFileTypes = SPDX2._SPDXFile_fileTypes file
        spdx2FileChecksums = SPDX2._SPDXFile_checksums file
        -- spdx2FileLicenseConcluded = SPDX2._SPDXFile_LicenseConcluded file
        -- spdx2FileLicenseInfoInFiles = SPDX2._SPDXFile_licenseInfoInFiles file
        -- spdx2FileLicenseInfoFromFiles = SPDX2._SPDXFile_licenseInfoFromFiles file
        -- spdx2FileLicenseComments = SPDX2._SPDXFile_licenseComments file
        -- spdx2FileCopyrightText = SPDX2._SPDXFile_copyrightText file
        spdx2FileComment = SPDX2._SPDXFile_comment file
        -- spdx2FileNoticeText = SPDX2._SPDXFile_noticeText file
        -- spdx2FileFileContributors = SPDX2._SPDXFile_fileContributors file
        -- spdx2FileAttributionTexts = SPDX2._SPDXFile_attributionTexts file
        -- spdx2FileFileDependencies = SPDX2._SPDXFile_fileDependencies file
        -- spdx2FileName = SPDX2._SPDXFile_name file

        spdx3FileType = case spdx2FileFileTypes of
            Nothing -> Nothing
            Just [] -> Nothing
            Just [ft] -> Just ("media-type-"++ show ft) -- TODO
            _ -> undefined

        spdx3ElementProperties = (emptyElement spdx2FileSPDXID ci) 
                                         { _elementName = Just spdx2FileFileName
                                         , _elementComment = spdx2FileComment
                                         }
        spdx3ArtifactProperties = ArtifactProperties []
        spdx3FileProperties = FileProperties Nothing [] spdx3FileType
    in File (Artifact (Element spdx3ElementProperties) spdx3ArtifactProperties) spdx3FileProperties

convertPackage :: CreationInfo -> SPDX2.SPDXPackage -> SPDX Package
convertPackage ci package = let
        spdx2PackageSPDXID = SPDX2._SPDXPackage_SPDXID package
        spdx2PackageName = SPDX2._SPDXPackage_name package
        spdx2PackageVersionInfo = SPDX2._SPDXPackage_versionInfo package
        spdx2PackagePackageFileName = SPDX2._SPDXPackage_packageFileName package
        spdx2PackageSupplier = SPDX2._SPDXPackage_supplier package
        spdx2PackageOriginator = SPDX2._SPDXPackage_originator package
        spdx2PackageDownloadLocation = SPDX2._SPDXPackage_downloadLocation package
        spdx2PackageFilesAnalyzed = SPDX2._SPDXPackage_filesAnalyzed package
        spdx2PackagePackageVerificationCode = SPDX2._SPDXPackage_packageVerificationCode package
        spdx2PackageChecksums = SPDX2._SPDXPackage_checksums package
        spdx2PackageHomepage = SPDX2._SPDXPackage_homepage package
        spdx2PackageSourceInfo = SPDX2._SPDXPackage_sourceInfo package
        -- spdx2PackageLicenseConcluded = SPDX2._SPDXPackage_licenseConcluded package
        -- spdx2PackageLicenseInfoFromFiles = SPDX2._SPDXPackage_licenseInfoFromFiles package
        -- spdx2PackageLicenseDeclared = SPDX2._SPDXPackage_licenseDeclared package
        -- spdx2PackageLicenseComments = SPDX2._SPDXPackage_licenseComments package
        -- spdx2PackageCopyrightText = SPDX2._SPDXPackage_copyrightText package
        spdx2PackageSummary = SPDX2._SPDXPackage_summary package
        spdx2PackageDescription = SPDX2._SPDXPackage_description package
        spdx2PackageComment = SPDX2._SPDXPackage_comment package
        spdx2PackageAttributionTexts = SPDX2._SPDXPackage_attributionTexts package
        spdx2PackageHasFiles = SPDX2._SPDXPackage_hasFiles package

        spdx3ElementProperties = (emptyElement spdx2PackageSPDXID ci) 
                                         { _elementName = Just spdx2PackageName
                                         , _elementComment = spdx2PackageComment
                                         }
        spdx3ArtifactProperties = ArtifactProperties []
        spdx3PackageProperties = PackageProperties Nothing [] (SPDX2.spdxMaybeToMaybe spdx2PackageDownloadLocation) Nothing spdx2PackageHomepage
    in Package (Artifact (Element spdx3ElementProperties) spdx3ArtifactProperties) spdx3PackageProperties

convertRelationship :: CreationInfo -> SPDX2.SPDXRelationship -> SPDX Relationship
convertRelationship ci rel = let
        spdx2RelationshipComment = SPDX2._SPDXRelationship_comment rel
        spdx2RelationshipRelationsihpType = SPDX2._SPDXRelationship_relationshipType rel
        spdx2RelationshipRelatedSpdxElement = SPDX2._SPDXRelationship_relatedSpdxElement rel
        spdx2RelationshipSpdxElementId = SPDX2._SPDXRelationship_spdxElementId rel

        spdx3RelationshipRelationshipType = case spdx2RelationshipRelationsihpType of
            SPDX2R.DESCRIBES -> Right DESCRIBES
            SPDX2R.DESCRIBED_BY -> Left DESCRIBES
            SPDX2R.CONTAINS -> Right CONTAINS
            SPDX2R.CONTAINED_BY -> Left CONTAINS
            SPDX2R.DEPENDS_ON -> Right DEPENDS_ON
            SPDX2R.DEPENDENCY_OF -> Left DEPENDS_ON
            SPDX2R.DEPENDENCY_MANIFEST_OF -> Left DEPENDENCY_MANIFEST
            SPDX2R.BUILD_DEPENDENCY_OF -> Left DEPENDENCY_MANIFEST
            SPDX2R.DEV_DEPENDENCY_OF -> Left DEV_DEPENDENCY
            -- SPDX2R.OPTIONAL_DEPENDENCY_OF -> OTHER
            -- SPDX2R.PROVIDED_DEPENDENCY_OF -> OTHER
            -- SPDX2R.TEST_DEPENDENCY_OF -> OTHER
            -- SPDX2R.RUNTIME_DEPENDENCY_OF -> OTHER
            -- SPDX2R.EXAMPLE_OF -> OTHER
            -- SPDX2R.GENERATES -> OTHER
            SPDX2R.GENERATED_FROM -> Left GENERATES
            -- SPDX2R.ANCESTOR_OF -> OTHER
            -- SPDX2R.DESCENDANT_OF -> OTHER
            -- SPDX2R.VARIANT_OF -> OTHER
            -- SPDX2R.DISTRIBUTION_ARTIFACT -> OTHER
            -- SPDX2R.PATCH_FOR -> OTHER
            -- SPDX2R.PATCH_APPLIED -> OTHER
            SPDX2R.COPY_OF -> Right COPY
            -- SPDX2R.FILE_ADDED -> OTHER
            -- SPDX2R.FILE_DELETED -> OTHER
            -- SPDX2R.FILE_MODIFIED -> OTHER
            -- SPDX2R.EXPANDED_FROM_ARCHIVE -> OTHER
            SPDX2R.DYNAMIC_LINK -> Right DYNAMIC_LINK
            -- SPDX2R.STATIC_LINK -> OTHER
            -- SPDX2R.DATA_FILE_OF -> OTHER
            -- SPDX2R.TEST_CASE_OF -> OTHER
            -- SPDX2R.BUILD_TOOL_OF -> OTHER
            -- SPDX2R.DEV_TOOL_OF -> OTHER
            -- SPDX2R.TEST_OF -> OTHER
            -- SPDX2R.TEST_TOOL_OF -> OTHER
            -- SPDX2R.DOCUMENTATION_OF -> OTHER
            -- SPDX2R.OPTIONAL_COMPONENT_OF -> OTHER
            -- SPDX2R.METAFILE_OF -> OTHER
            -- SPDX2R.PACKAGE_OF -> OTHER
            -- SPDX2R.AMENDS -> OTHER
            -- SPDX2R.PREREQUISITE_FOR -> OTHER
            -- SPDX2R.HAS_PREREQUISITE -> OTHER
            -- SPDX2R.OTHER -> OTHER
            t -> error ("failed to translate RelationsihpType=" ++ show t)

        spdx3ElementProperties spdxid = (emptyElement spdxid ci) {  _elementComment = spdx2RelationshipComment }
        spdx3RelationshipProperties = case spdx3RelationshipRelationshipType of
            Right rt -> RelationshipProperties rt (Ref spdx2RelationshipRelatedSpdxElement) [Ref spdx2RelationshipSpdxElementId] Nothing
            Left rt -> RelationshipProperties rt (Ref spdx2RelationshipSpdxElementId) [Ref spdx2RelationshipRelatedSpdxElement] Nothing
    in setSPDXIDFromContent (\spdxid -> Relationship (Element (spdx3ElementProperties spdxid)) spdx3RelationshipProperties)

convertDocument :: SPDX2.SPDXDocument -> Either String (SPDX ())
convertDocument doc = let
        spdx2DocumentId = SPDX2._SPDX_SPDXID doc
        spdx2Comment = SPDX2._SPDX_comment doc
        spdx2CreationInfo = SPDX2._SPDX_creationInfo doc
        spdx2DataLicense = SPDX2._SPDX_dataLicense doc
        spdx2Name = SPDX2._SPDX_name doc

        spdx2files = SPDX2._SPDX_files doc
        spdx2packages = SPDX2._SPDX_packages doc
        spdx2relationships = SPDX2._SPDX_relationships doc

        (spdx3CreationInfo, _) = convertCreationInfo spdx2CreationInfo spdx2DataLicense
    in runSPDX spdx3CreationInfo $
        spdxDocument spdx2Name $
            collection' spdx2DocumentId $ do
            let packages = map (pack . convertPackage spdx3CreationInfo) spdx2packages
            let files = map (pack . convertFile spdx3CreationInfo) spdx2files
            let relationships = map (pack . convertRelationship spdx3CreationInfo) spdx2relationships
            return (packages ++ files ++ relationships)

convertBsDocument :: BSL.ByteString -> Either String (SPDX ())
convertBsDocument = (\case
        Left err -> Left err
        Right doc -> convertDocument doc
    ) . SPDX2.parseSPDXDocumentBS