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
import qualified Data.ByteString.Lazy            as BSL
import           Data.Maybe                      (fromMaybe, maybeToList)
import           Data.Time.Format.ISO8601
import           SPDX3.Model
import           SPDX3.Monad

import qualified SPDX.Document                   as SPDX2
import qualified SPDX.Document.RelationshipTypes as SPDX2R

parseSpdx2Actor :: String -> Actor
parseSpdx2Actor ('P':'e':'r':'s':'o':'n':':':' ':name) = Actor (Just name) (Just PERSON)
parseSpdx2Actor ('O':'r':'g':'a':'n':'i':'z':'a':'t':'i':'o':'n':':':' ':name) = Actor (Just name) (Just ORGANIZATION)
parseSpdx2Actor ('T':'o':'o':'l':':':' ':name) = Actor (Just name) (Just TOOL)
parseSpdx2Actor name = Actor (Just name) Nothing


convertCreationInfo :: SPDX2.SPDXCreationInfo -> String -> (CreationInfo, Maybe String)
convertCreationInfo spdx2CreationInformation spdx2DataLicense = let
      spdx2CreationInfoComment = SPDX2._SPDXCreationInfo_comment spdx2CreationInformation
      spdx2CreationInfoCreated = SPDX2._SPDXCreationInfo_created spdx2CreationInformation
      spdx2CreationInfoCreators = SPDX2._SPDXCreationInfo_creators spdx2CreationInformation
      spdx2CreationInfoLicenseListVersion = SPDX2._SPDXCreationInfo_licenseListVersion spdx2CreationInformation

      spdx3CreationInfoCreated = let
            fallback = fromMaybe undefined (iso8601ParseM "2022-11-13T13:14:36.324980945Z")
        in fromMaybe fallback (iso8601ParseM spdx2CreationInfoCreated)
    in ((CreationInfo
            { _specVer = "3.0.0"
            , _profile = ["core", "software", "licensing"]
            , _created = spdx3CreationInfoCreated
            , _dataLicense = case spdx2DataLicense of
                "CC0-1.0" -> CC0
                _         -> undefined
            , _createdBy = map parseSpdx2Actor spdx2CreationInfoCreators
            }), spdx2CreationInfoLicenseListVersion)

convertFile :: SPDX2.SPDXFile -> SPDX_M (SPDX ())
convertFile spdx2File = let
        spdx2FileSPDXID = SPDX2._SPDXFile_SPDXID spdx2File
        spdx2FileFileName = SPDX2._SPDXFile_fileName spdx2File
        spdx2FileFileTypes = SPDX2._SPDXFile_fileTypes spdx2File
        spdx2FileChecksums = SPDX2._SPDXFile_checksums spdx2File
        -- spdx2FileLicenseConcluded = SPDX2._SPDXFile_LicenseConcluded spdx2File
        -- spdx2FileLicenseInfoInFiles = SPDX2._SPDXFile_licenseInfoInFiles spdx2File
        -- spdx2FileLicenseInfoFromFiles = SPDX2._SPDXFile_licenseInfoFromFiles spdx2File
        -- spdx2FileLicenseComments = SPDX2._SPDXFile_licenseComments spdx2File
        -- spdx2FileCopyrightText = SPDX2._SPDXFile_copyrightText spdx2File
        spdx2FileComment = SPDX2._SPDXFile_comment spdx2File
        -- spdx2FileNoticeText = SPDX2._SPDXFile_noticeText spdx2File
        -- spdx2FileFileContributors = SPDX2._SPDXFile_fileContributors spdx2File
        -- spdx2FileAttributionTexts = SPDX2._SPDXFile_attributionTexts spdx2File
        -- spdx2FileFileDependencies = SPDX2._SPDXFile_fileDependencies spdx2File
        -- spdx2FileName = SPDX2._SPDXFile_name spdx2File

        spdx3VerifiedUsing = map (\ (SPDX2.SPDXChecksum spdx2Algorithm spdx2ChecksumValue) -> mkHash (show spdx2Algorithm) spdx2ChecksumValue) spdx2FileChecksums
        spdx3FileType = case spdx2FileFileTypes of
            Nothing   -> Nothing
            Just []   -> Nothing
            Just [ft] -> Just ("media-type-"++ show ft) -- TODO
            _         -> undefined

        spdx3ElementProperties = def { _elementName = Just spdx2FileFileName
                                     , _elementComment = spdx2FileComment
                                     , _elementVerifiedUsing = spdx3VerifiedUsing
                                     }
        spdx3ArtifactProperties = ArtifactProperties []
        spdx3FileProperties = FileProperties Nothing [] spdx3FileType
    in file (Just spdx2FileSPDXID) spdx3FileProperties spdx3ArtifactProperties spdx3ElementProperties


convertPackage :: SPDX2.SPDXPackage -> SPDX_M (SPDX (), [SPDX ()])
convertPackage spdx2Package = let
        spdx2PackageSPDXID = SPDX2._SPDXPackage_SPDXID spdx2Package
        spdx2PackageName = SPDX2._SPDXPackage_name spdx2Package
        spdx2PackageVersionInfo = SPDX2._SPDXPackage_versionInfo spdx2Package
        spdx2PackagePackageFileName = SPDX2._SPDXPackage_packageFileName spdx2Package
        spdx2PackageSupplier = SPDX2._SPDXPackage_supplier spdx2Package
        spdx2PackageOriginator = SPDX2._SPDXPackage_originator spdx2Package
        spdx2PackageDownloadLocation = SPDX2._SPDXPackage_downloadLocation spdx2Package
        spdx2PackageFilesAnalyzed = SPDX2._SPDXPackage_filesAnalyzed spdx2Package
        spdx2PackagePackageVerificationCode = SPDX2._SPDXPackage_packageVerificationCode spdx2Package
        spdx2PackageChecksums = SPDX2._SPDXPackage_checksums spdx2Package
        spdx2PackageHomepage = SPDX2._SPDXPackage_homepage spdx2Package
        spdx2PackageSourceInfo = SPDX2._SPDXPackage_sourceInfo spdx2Package
        -- spdx2PackageLicenseConcluded = SPDX2._SPDXPackage_licenseConcluded spdx2Package
        -- spdx2PackageLicenseInfoFromFiles = SPDX2._SPDXPackage_licenseInfoFromFiles spdx2Package
        -- spdx2PackageLicenseDeclared = SPDX2._SPDXPackage_licenseDeclared spdx2Package
        -- spdx2PackageLicenseComments = SPDX2._SPDXPackage_licenseComments spdx2Package
        -- spdx2PackageCopyrightText = SPDX2._SPDXPackage_copyrightText spdx2Package
        spdx2PackageSummary = SPDX2._SPDXPackage_summary spdx2Package
        spdx2PackageDescription = SPDX2._SPDXPackage_description spdx2Package
        spdx2PackageComment = SPDX2._SPDXPackage_comment spdx2Package
        -- spdx2PackageAttributionTexts = SPDX2._SPDXPackage_attributionTexts spdx2Package
        spdx2PackageHasFiles = SPDX2._SPDXPackage_hasFiles spdx2Package

        spdx3VerifiedUsing = let
            fromChecksums = map (\ (SPDX2.SPDXChecksum spdx2Algorithm spdx2ChecksumValue) -> mkHash (show spdx2Algorithm) spdx2ChecksumValue) spdx2PackageChecksums
            fromVerificationCode = case spdx2PackagePackageVerificationCode of
                Just (SPDX2.SPDXPackageVerificationCode packageVerificationHex _) -> [mkHash "packageVerificationCode" packageVerificationHex ]
                Nothing -> []
            in fromChecksums ++ fromVerificationCode
        spdx3ElementProperties = def { _elementName = Just spdx2PackageName
                                     , _elementComment = spdx2PackageComment
                                     , _elementSummary = spdx2PackageSummary
                                     , _elementDescription = spdx2PackageDescription
                                     , _elementVerifiedUsing = spdx3VerifiedUsing
                                     }
        spdx3ArtifactProperties = def {_artifactOriginatedBy = map parseSpdx2Actor (maybeToList spdx2PackageOriginator)}
        spdx3PackageProperties = def { _downloadLocation = SPDX2.spdxMaybeToMaybe spdx2PackageDownloadLocation
                                     , _packageHomePage = spdx2PackageHomepage
                                     }
    in do
        p <- package (Just spdx2PackageSPDXID) spdx3PackageProperties spdx3ArtifactProperties spdx3ElementProperties

        spdx3FileCorrespondingToPackage <- case spdx2PackagePackageFileName of
            Just _ -> do
                f <- file Nothing def def def{_elementName = spdx2PackagePackageFileName}
                (:[]) <$> relationship (Just "The File of that packages is") (RelationshipProperties PACKAGES f [Ref spdx2PackageSPDXID] Nothing) def
            Nothing -> return []
        spdx3FromHasFiles <- case spdx2PackageHasFiles of
            Just spdx2PackageHasFiles' -> do
                let fileRefs = map Ref spdx2PackageHasFiles'
                (:[]) <$> relationship Nothing (RelationshipProperties CONTAINS (Ref spdx2PackageSPDXID) fileRefs Nothing) def
            Nothing -> return []

        return (p, spdx3FileCorrespondingToPackage ++ spdx3FromHasFiles)

convertRelationship :: SPDX2.SPDXRelationship -> SPDX_M (SPDX ())
convertRelationship spdx2Relationship = let
        spdx2RelationshipComment = SPDX2._SPDXRelationship_comment spdx2Relationship
        spdx2RelationshipRelationsihpType = SPDX2._SPDXRelationship_relationshipType spdx2Relationship
        spdx2RelationshipRelatedSpdxElement = SPDX2._SPDXRelationship_relatedSpdxElement spdx2Relationship
        spdx2RelationshipSpdxElementId = SPDX2._SPDXRelationship_spdxElementId spdx2Relationship

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
            -- SPDX2R.OPTIONAL_DEPENDENCY_OF -> undefined
            -- SPDX2R.PROVIDED_DEPENDENCY_OF -> undefined
            -- SPDX2R.TEST_DEPENDENCY_OF -> undefined
            -- SPDX2R.RUNTIME_DEPENDENCY_OF -> undefined
            -- SPDX2R.EXAMPLE_OF -> undefined
            -- SPDX2R.GENERATES -> undefined
            SPDX2R.GENERATED_FROM -> Left GENERATES
            -- SPDX2R.ANCESTOR_OF -> undefined
            -- SPDX2R.DESCENDANT_OF -> undefined
            -- SPDX2R.VARIANT_OF -> undefined
            -- SPDX2R.DISTRIBUTION_ARTIFACT -> undefined
            -- SPDX2R.PATCH_FOR -> undefined
            -- SPDX2R.PATCH_APPLIED -> undefined
            SPDX2R.COPY_OF -> Right COPY
            -- SPDX2R.FILE_ADDED -> undefined
            -- SPDX2R.FILE_DELETED -> undefined
            -- SPDX2R.FILE_MODIFIED -> undefined
            -- SPDX2R.EXPANDED_FROM_ARCHIVE -> undefined
            SPDX2R.DYNAMIC_LINK -> Right DYNAMIC_LINK
            -- SPDX2R.STATIC_LINK -> undefined
            -- SPDX2R.DATA_FILE_OF -> undefined
            -- SPDX2R.TEST_CASE_OF -> undefined
            -- SPDX2R.BUILD_TOOL_OF -> undefined
            -- SPDX2R.DEV_TOOL_OF -> undefined
            -- SPDX2R.TEST_OF -> undefined
            -- SPDX2R.TEST_TOOL_OF -> undefined
            -- SPDX2R.DOCUMENTATION_OF -> undefined
            -- SPDX2R.OPTIONAL_COMPONENT_OF -> undefined
            -- SPDX2R.METAFILE_OF -> undefined
            -- SPDX2R.PACKAGE_OF -> undefined
            -- SPDX2R.AMENDS -> undefined
            -- SPDX2R.PREREQUISITE_FOR -> undefined
            -- SPDX2R.HAS_PREREQUISITE -> undefined
            -- SPDX2R.OTHER -> undefined
            SPDX2R.SPECIFICATION_FOR -> Right OTHER -- TODO!
            t -> error ("failed to translate RelationsihpType=" ++ show t)

        spdx3ElementProperties = def {  _elementComment = spdx2RelationshipComment }
        spdx3RelationshipProperties = case spdx3RelationshipRelationshipType of
            Right rt -> RelationshipProperties rt (Ref spdx2RelationshipRelatedSpdxElement) [Ref spdx2RelationshipSpdxElementId] Nothing
            Left rt -> RelationshipProperties rt (Ref spdx2RelationshipSpdxElementId) [Ref spdx2RelationshipRelatedSpdxElement] Nothing
    in relationship Nothing spdx3RelationshipProperties spdx3ElementProperties

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
    in runSPDX spdx3CreationInfo $ do
        packages <- concatMap (\(p, fs) -> pack p : fs) <$> mapM convertPackage spdx2packages
        files <- map pack <$> mapM convertFile spdx2files
        relationships <- map pack <$> mapM convertRelationship spdx2relationships
        let elements = packages ++ files ++ relationships
        spdxDocument (Just spdx2DocumentId) def def{_collectionElements = elements} def{_elementName = Just spdx2Name, _elementComment = spdx2Comment}

convertBsDocument :: BSL.ByteString -> Either String (SPDX ())
convertBsDocument = (\case
        Left err  -> Left err
        Right doc -> convertDocument doc
    ) . SPDX2.parseSPDXDocumentBS
