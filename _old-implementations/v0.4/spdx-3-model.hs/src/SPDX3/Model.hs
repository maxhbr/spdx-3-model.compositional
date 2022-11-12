{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module SPDX3.Model
    where
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.KeyMap as KM
import SPDX3.SPDXID
import SPDX3.CreationInfo
import SPDX3.RelationshipType
import Data.Data
import qualified Data.Text as T
import Data.Dynamic
import qualified Data.HashMap.Strict as Map
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.ByteString (ByteString)
import GHC.Word (Word8)

-- TODOs:
type SoftwarePurpose = String
type IRI = String
type URL = String
type URI = String
type MediaType = String
type RelationshipCompleteness = String

-- -- ############################################################################
-- -- ##  Element  ###############################################################
-- -- ############################################################################

type HashAlgorithm  = String
data IntegrityMethod where
    Hash :: Maybe String -> HashAlgorithm -> [Word8] -> IntegrityMethod
  deriving (Generic, Show)
instance ToJSON IntegrityMethod where
    toJSON (Hash comment algorithm hashValue) = object ["comment" .= comment
                                                       ,"algorithm" .= algorithm
                                                       ,"hashValue" .= hashValue
                                                       ]
instance FromJSON IntegrityMethod where
    parseJSON = withObject "IntegrityMethod" $ \o -> do
        Hash <$> o .:? "comment"
             <*> o .: "algorithm"
             <*> o .: "hashValue"

type ExternalReferenceType = String -- TODO
data ExternalReference where
  ExternalReference :: {_externalReferenceType :: ExternalReferenceType
                       ,_externalReferenceLocator :: IRI
                       ,_externalReferenceContentType :: Maybe MediaType
                       ,_externalReferenceComments :: Maybe String
                       } -> ExternalReference
  deriving (Generic, Show)
instance ToJSON ExternalReference where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ExternalReference

type ExternalIdentifierType = String -- TODO
data ExternalIdentifier where
  ExternalIdentifier :: {_externalIdentifierType :: ExternalIdentifierType
                        ,_externalIdentifierIdentifier :: IRI
                        ,_externalIdentifierComments :: Maybe String
                        } -> ExternalIdentifier
  deriving (Generic, Show)
instance ToJSON ExternalIdentifier where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ExternalIdentifier

data Element where
  ElementProperties :: {_elementName :: Maybe String,
                        _elementSummary :: Maybe String,
                        _elementDescription :: Maybe String,
                        _elementComment :: Maybe String,
                        _elementVerifiedUsing :: [IntegrityMethod],
                        _elementExternalReferences :: [ExternalReference],
                        _elementExternalIdentifiers :: [ExternalIdentifier]
                        } -> Element
  deriving Show
emptyElement :: Element
emptyElement = ElementProperties Nothing Nothing Nothing Nothing mempty mempty mempty
elementFromName :: String -> Element
elementFromName name = ElementProperties (Just name) Nothing Nothing Nothing mempty mempty mempty
instance ToJSON Element where
    toJSON (ElementProperties name summary description comment verifiedUsing externalReferences externalIdentifiers) =
        object [ "name" .= name
               , "summary" .= summary 
               , "description" .= description 
               , "comment" .= comment
               , "verifiedUsing" .= verifiedUsing
               , "externalReferences" .= externalReferences
               , "externalIdentifiers" .= externalIdentifiers
               ]
instance FromJSON Element where
    parseJSON = withObject "ElementProperties" $ \o -> do
        ElementProperties <$> o .:? "name"
                          <*> o .:? "summary"
                          <*> o .:? "description"
                          <*> o .:? "comment"
                          <*> o .: "verifiedUsing"
                          <*> o .: "externalReferences"
                          <*> o .: "externalIdentifiers"

data Artifact where
    ArtifactProperties :: {_artifactOriginatedBy :: [Actor]} -> Artifact
  deriving (Show)
instance ToJSON Artifact where
    toJSON (ArtifactProperties originatedBy) = object [ "originatedBy" .= originatedBy ]
instance FromJSON Artifact where
    parseJSON = withObject "ArtifactProperties" $ \o -> do
        ArtifactProperties <$> o .: "originatedBy"
type NamespaceMap = Map.HashMap String IRI
type ExternalMap = Map.HashMap IRI (Maybe URL, IntegrityMethod)
data Collection where
  CollectionProperties :: {_collectionElements :: [SPDX ()]
                          ,_collectionRootElements :: [SPDX ()]
                          ,_collectionNamespaces :: NamespaceMap
                          ,_collectionImports :: ExternalMap
                          } -> Collection
  deriving (Show)
instance ToJSON Collection where
    toJSON (CollectionProperties elements rootElements namespaces imports) =
         object [ "elements" .= elements
                , "rootElements" .= rootElements
                , "namespaces" .= namespaces
                , "imports" .= imports
                ]
instance FromJSON Collection where
    parseJSON = withObject "CollectionProperties" $ \o -> do
        CollectionProperties <$> o .: "elements"
                             <*> o .: "rootElements"
                             <*> o .: "namespaces"
                             <*> o .: "imports"
data Bundle where
    BundleProperties :: {_bundleContext :: Maybe String
                        } -> Bundle
  deriving (Show)
instance ToJSON Bundle where
    toJSON (BundleProperties context) =
         object [ "context" .= context]
instance FromJSON Bundle where
    parseJSON = withObject "BundleProperties" $ \o -> do
        BundleProperties <$> o .: "context"
data BOM
data Relationship where
    RelationshipProperties :: {_relationshipType :: RelationshipType
                              ,_relationshipFrom :: SPDX ()
                              ,_relationshipTo :: [SPDX ()]
                              ,_relationshipCompleteness :: Maybe RelationshipCompleteness
                              } -> Relationship
  deriving (Show)
instance ToJSON Relationship where
    toJSON (RelationshipProperties t from to relationshipCompleteness) =
         object [ "relationshipType" .= t , "from" .= from , "to" .= to, "relationshipCompleteness" .= relationshipCompleteness ]
instance FromJSON Relationship where
    parseJSON = withObject "RelationshipProperties" $ \o -> do
        RelationshipProperties <$> o .: "relationshipType" <*> o .: "from" <*> o .: "to" <*> o .: "relationshipCompleteness"
data Annotation where
    AnnotationProperties :: {_annotationStatement :: String
                            ,_annotationSubject :: SPDX ()
                            } -> Annotation
  deriving (Show)
instance ToJSON Annotation where
    toJSON (AnnotationProperties statement subject) = object [ "statement" .= statement , "subject" .= subject ]
instance FromJSON Annotation where
    parseJSON = withObject "AnnotationProperties" $ \o -> do
        AnnotationProperties <$> o .: "statement" <*> o .: "subject"

-- Software
data Package where
    PackageProperties :: {_packageContentIdentifier :: Maybe URI
                         , _packagePurpose :: [SoftwarePurpose]
                         , _downloadLocation :: Maybe URL
                         , _packageUrl :: Maybe URL
                         , _packageHomePage :: Maybe URL
                         } -> Package
  deriving (Show)
instance ToJSON Package where
    toJSON (PackageProperties contentIdentifier packagePurpose downloadLocation packageUrl homePage) =
         object [ "contentIdentifier" .= contentIdentifier
                , "packagePurpose" .= packagePurpose
                , "downloadLocation" .= downloadLocation
                , "packageUrl" .= packageUrl
                , "homePage" .= homePage
                ]
instance FromJSON Package where
    parseJSON = withObject "PackageProperties" $ \o -> do
        PackageProperties <$> o .:? "contentIdentifier"
                          <*> o .: "packagePurpose"
                          <*> o .:? "downloadLocation"
                          <*> o .:? "packageUrl"
                          <*> o .:? "homePage"
data File where
    FileProperties :: {_fileContentIdentifier :: Maybe String
                      ,_filePurpose :: [SoftwarePurpose]
                      , _fileContentType :: Maybe MediaType
                      } -> File
  deriving (Show)
instance ToJSON File where
    toJSON (FileProperties contentIdentifier filePurpose contentType) =
         object [ "contentIdentifier" .= contentIdentifier
                , "filePurpose" .= filePurpose
                , "contentType" .= contentType
                ]
instance FromJSON File where
    parseJSON = withObject "FileProperties" $ \o -> do
        FileProperties <$> o .:? "contentIdentifier"
                          <*> o .: "filePurpose"
                          <*> o .:? "contentType"
data Snippet where
    SnippetProperties :: {_snippetContentIdentifier :: Maybe String
                         ,_snippetPurpose :: [SoftwarePurpose]
                         ,_snippetByteRange :: Maybe (Int,Int)
                         ,_snippetLineRange :: Maybe (Int,Int)
                         } -> Snippet
  deriving (Show)
instance ToJSON Snippet where
    toJSON (SnippetProperties contentIdentifier snippetPurpose byteRange lineRange) =
         object [ "contentIdentifier" .= contentIdentifier
                , "snippetPurpose" .= snippetPurpose
                , "byteRange" .= byteRange
                , "lineRange" .= lineRange
                ]
instance FromJSON Snippet where
    parseJSON = withObject "SnippetProperties" $ \o -> do
        SnippetProperties <$> o .:? "contentIdentifier"
                          <*> o .: "snippetPurpose"
                          <*> o .:? "byteRange"
                          <*> o .:? "lineRange"
data SBOM

data SPDX a where
    Ref          :: SPDXID -> SPDX ()
    Pack         :: SPDX a -> SPDX ()

    Element      :: SPDXID -> CreationInfo -> Element -> SPDX Element

    Artifact     :: SPDX Element -> Artifact -> SPDX Artifact
    Collection   :: SPDX Element -> Collection -> SPDX Collection
    Bundle       :: SPDX Collection -> Bundle -> SPDX Bundle
    BOM          :: SPDX Bundle -> SPDX BOM
    Relationship :: SPDX Element -> Relationship -> SPDX Relationship
    Annotation   :: SPDX Element -> Annotation -> SPDX Annotation

    -- Software
    Package      :: SPDX Artifact -> Package -> SPDX Package
    File         :: SPDX Artifact -> File    -> SPDX File
    Snippet      :: SPDX Artifact -> Snippet -> SPDX Snippet
    SBOM         :: SPDX BOM -> SPDX SBOM
deriving instance Show (SPDX a)

pack :: SPDX a -> SPDX ()
pack e@(Ref _) = e
pack e@(Pack _) = e
pack e = Pack e

instance HasSPDXID (SPDX a) where
    getSPDXID (Ref i) = i
    getSPDXID (Pack e) = getSPDXID e

    getSPDXID (Element i _ _) = i

    getSPDXID (Artifact ie _) = getSPDXID ie
    getSPDXID (Collection ie _) = getSPDXID ie
    getSPDXID (Bundle c _) = getSPDXID c
    getSPDXID (BOM b) = getSPDXID b
    getSPDXID (Relationship ie _) = getSPDXID ie
    getSPDXID (Annotation ie _) = getSPDXID ie

    getSPDXID (Package a _) = getSPDXID a
    getSPDXID (File a _) = getSPDXID a
    getSPDXID (Snippet a _) = getSPDXID a
    getSPDXID (SBOM b) = getSPDXID b


getElements :: SPDX a -> [SPDX ()]
getElements e = let
        getImplicitElements' :: SPDX a -> [SPDX ()]
        getImplicitElements' (Pack e) = getImplicitElements e
        getImplicitElements' e@(Collection _ (CollectionProperties es res _ _)) = concatMap getImplicitElements es ++ concatMap getImplicitElements res
        getImplicitElements' e@(Relationship _ (RelationshipProperties _ from to _)) = concatMap getImplicitElements (from:to)
        getImplicitElements' e@(Annotation _ (AnnotationProperties _ subject)) = concatMap getImplicitElements [subject]
        getImplicitElements' _ = []
        getImplicitElements :: forall a. SPDX a -> [SPDX ()]
        getImplicitElements e = getImplicitElements' e ++ maybe [] getImplicitElements (getParent e)
    in pack e : getImplicitElements e

getSPDXIDs :: SPDX a -> [SPDXID]
getSPDXIDs  = map getSPDXID . getElements

getType :: SPDX a -> String
getType (Ref _) = "Ref"
getType (Pack e) = getType e

getType (Element {}) = "Element"

getType (Artifact {}) = "Artifact"
getType (Collection {}) = "Collection"
getType (Bundle {}) = "Bundle"
getType (BOM {}) = "BOM"
getType (Relationship {}) = "Relationship"
getType (Annotation {}) = "Annotation"

getType (Package {}) = "Package"
getType (File {}) = "File"
getType (Snippet {}) = "Snippet"
getType (SBOM {}) = "SBOM"

getParent :: SPDX a -> Maybe (SPDX ())
getParent (Ref _)             = Nothing

getParent (Pack e)            = getParent e
getParent (Element {})        = Nothing

getParent (Artifact ie _)     = Just $ pack ie
getParent (Collection ie _)   = Just $ pack ie
getParent (Bundle c _)        = Just $ pack c
getParent (BOM b)             = Just $ pack b
getParent (Relationship ie _) = Just $ pack ie
getParent (Annotation ie _)   = Just $ pack ie

getParent (Package b _)       = Just $ pack b
getParent (File    b _)       = Just $ pack b
getParent (Snippet b _)       = Just $ pack b
getParent (SBOM b)            = Just $ pack b

getJsons :: SPDX a -> [Value]
getJsons (Ref i) = undefined
getJsons (Pack e) = getJsons e

getJsons (Element i ci e) = [object [ "SPDXID" .= i , "creationInfo" .= ci ], toJSON  e]

getJsons (Artifact _ aps) =  [toJSON aps]
getJsons (Collection _ cps) = [toJSON cps]
getJsons (Bundle _ bps) = [toJSON bps]
getJsons (BOM _) = []
getJsons (Relationship _ rps) = [ toJSON rps ]
getJsons (Annotation _ aps) = [ toJSON aps ]

getJsons (Package _ pps) = [toJSON pps]
getJsons (File _ fps) = [toJSON fps]
getJsons (Snippet _ sps) = [toJSON sps]
getJsons (SBOM _) = []

instance ToJSON (SPDX a) where
    toJSON (Ref i) = toJSON i
    toJSON e = let
          t = object ["@type" .= getType e]
          parent = case getParent e of
            Just parent -> [toJSON parent]
            Nothing -> []
          mergeObjects :: [Value] -> Value
          mergeObjects list = let
              unObject :: Value -> Object
              unObject (Object o) = o
              unObject _ = undefined -- partial function :see_no_evil:
            in Object (mconcat (map unObject list))
        in mergeObjects $ t : getJsons e ++ parent

parseElementJSON :: Value -> Parser (SPDX Element)
parseElementJSON = withObject "Element" $ \o -> do
    Element <$> o .: "SPDXID"
            <*> o .: "creationInfo"
            <*> parseJSON (Object o)
parseArtifactJSON :: Value -> Parser (SPDX Artifact)
parseArtifactJSON = withObject "Artifact" $ \o -> do
    Artifact <$> parseElementJSON (Object o)
             <*> parseJSON (Object o)
parseCollectionJSON :: Value -> Parser (SPDX Collection)
parseCollectionJSON = withObject "Collection" $ \o -> do
    Collection <$> parseElementJSON (Object o)
               <*> parseJSON (Object o)
parseRelationshipJSON :: Value -> Parser (SPDX Relationship)
parseRelationshipJSON = withObject "Relationship" $ \o -> do
    Relationship <$> parseElementJSON (Object o)
                 <*> parseJSON (Object o)
parseAnnotationJSON :: Value -> Parser (SPDX Annotation)
parseAnnotationJSON = withObject "Annotation" $ \o -> do
    Annotation <$> parseElementJSON (Object o)
               <*> parseJSON (Object o)
instance FromJSON (SPDX ()) where
    parseJSON (String s) = return $ Ref (T.unpack s)
    parseJSON o@(Object v) = ((v .: "@type") :: Parser String) >>= \case
        "Ref" -> fail "Ref is not an object"
        "Element" -> pack <$> parseElementJSON o
        "Artifact" -> pack <$> parseArtifactJSON o
        "Collection" -> pack <$> parseCollectionJSON o
        "Relationship" -> pack <$> parseRelationshipJSON o
        "Annotation" -> pack <$> parseAnnotationJSON o
        t -> fail ("type='" ++ t ++ "' not supported")
    parseJSON _ = fail "not supported type"

-- ############################################################################
-- ##  monad  #################################################################
-- ############################################################################

type SPDX_M a = Reader CreationInfo a

ref :: SPDXID -> SPDX_M (SPDX ())
ref = return . Ref

artifact' :: SPDXID -> String -> SPDX_M (SPDX Artifact)
artifact' i name = do
    ci <- ask
    let ie = Element i ci (elementFromName name)
    return (Artifact ie (ArtifactProperties []))
artifact :: SPDXID -> String -> SPDX_M (SPDX ())
artifact i name = pack <$> artifact' i name

collection' :: SPDXID -> SPDX_M [SPDX ()] -> SPDX_M (SPDX Collection)
collection' i es = do
    ci <- ask
    let ie = Element i ci emptyElement
    Collection ie . (\es -> CollectionProperties es [] mempty mempty) <$> es
collection :: SPDXID -> SPDX_M [SPDX ()] -> SPDX_M (SPDX ())
collection i es = pack <$> collection' i es

annotation' :: SPDXID -> String -> SPDX_M (SPDX ()) -> SPDX_M (SPDX Annotation)
annotation' i stmnt subject = do
    ci <- ask
    let ie = Element i ci emptyElement
    Annotation ie . AnnotationProperties stmnt <$> subject
annotation :: SPDXID -> String -> SPDX_M (SPDX ()) -> SPDX_M (SPDX ())
annotation i stmnt subject = pack <$> annotation' i stmnt subject

runSPDX :: CreationInfo -> SPDX_M a -> a
runSPDX ci = (`runReader` ci)

-- ############################################################################
-- ##  example  ###############################################################
-- ############################################################################

mkExample :: IO (SPDX ())
mkExample = do
    let actors = [Actor (Just "Some Actor") (Just PERSON), Actor (Just "This Tool") (Just TOOL)]
    creationInfo <- mkCreationInfo actors
    return . runSPDX creationInfo $
        collection "urn:spdx:Collection0" $ do
            r0 <- ref "urn:spdx:Ref0"
            r1 <- ref "urn:spdx:Ref1"
            r2 <- ref "urn:spdx:Ref2"
            a0 <- artifact "urn:spdx:Artifact0" "Artifact0"
            c1 <- collection "urn:spdx:Collection1" $ do
                an0 <- annotation "urn:spdx:Annotation0" "Some Annotation" $ do
                    ref "urn:spdx:Artifact0"
                return [an0]
            return [r0,r1,r2,a0,c1]
