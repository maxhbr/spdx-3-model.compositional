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

module SPDX3.Model
    ( module X
    , Element (..)
    , Artifact (..)
    , Collection (..)
    , Bundle (..)
    , BOM (..)
    , SpdxDocument (..)
    , Relationship (..)
    , Annotation (..)
    , Package (..)
    , File (..)
    , Snippet (..)
    , SBOM (..)
    , SPDX (..)
    , pack
    , toRef
    , getType
    , getParent
    , setSPDXIDFromContent
    )
    where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default                   as X (Default (..))
import           Data.Digest.Pure.MD5           (md5)
import qualified Data.HashMap.Strict            as Map
import qualified Data.Text                      as T
import qualified Purl.Purl                      as Purl
import           SPDX3.Model.Common             as X
import           SPDX3.Model.CreationInfo       as X
import           SPDX3.Model.ExternalIdentifier as X
import           SPDX3.Model.ExternalReference  as X
import           SPDX3.Model.IntegrityMethod    as X
import           SPDX3.Model.RelationshipType   as X
import           SPDX3.Model.SPDXID             as X

-- -- ############################################################################
-- -- ##  Element  ###############################################################
-- -- ############################################################################

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
instance Default Element where
    def = ElementProperties Nothing Nothing Nothing Nothing mempty mempty mempty
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
instance Default Artifact where
    def = ArtifactProperties []
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
instance Default Collection where
    def = CollectionProperties mempty mempty mempty mempty
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
instance Default Bundle where
    def = BundleProperties Nothing
instance ToJSON Bundle where
    toJSON (BundleProperties context) =
         object [ "context" .= context]
instance FromJSON Bundle where
    parseJSON = withObject "BundleProperties" $ \o -> do
        BundleProperties <$> o .: "context"
data SpdxDocument
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
                         , _packageUrl :: Maybe Purl.Purl
                         , _packageHomePage :: Maybe URL
                         } -> Package
  deriving (Show)
instance Default Package where
    def = PackageProperties Nothing [] Nothing Nothing Nothing
instance ToJSON Package where
    toJSON (PackageProperties contentIdentifier packagePurpose downloadLocation packageUrl homePage) =
         object [ "contentIdentifier" .= contentIdentifier
                , "packagePurpose" .= packagePurpose
                , "downloadLocation" .= downloadLocation
                , "packageUrl" .= fmap show packageUrl
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
                      , _fileContentType :: [MediaType]
                      } -> File
  deriving (Show)
instance Default File where
    def = FileProperties Nothing mempty mempty
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
                       <*> ((\case 
                                Just a -> a
                                Nothing -> []) <$> o .:? "contentType")
data Snippet where
    SnippetProperties :: {_snippetContentIdentifier :: Maybe String
                         ,_snippetPurpose :: [SoftwarePurpose]
                         ,_snippetByteRange :: Maybe (Int,Int)
                         ,_snippetLineRange :: Maybe (Int,Int)
                         } -> Snippet
  deriving (Show)
instance Default Snippet where
    def = SnippetProperties Nothing [] Nothing Nothing
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
    SpdxDocument :: SPDX Bundle -> SPDX SpdxDocument
    Relationship :: SPDX Element -> Relationship -> SPDX Relationship
    Annotation   :: SPDX Element -> Annotation -> SPDX Annotation

    -- Software
    Package      :: SPDX Artifact -> Package -> SPDX Package
    File         :: SPDX Artifact -> File    -> SPDX File
    Snippet      :: SPDX Artifact -> Snippet -> SPDX Snippet
    SBOM         :: SPDX BOM -> SPDX SBOM
deriving instance Show (SPDX a)

pack :: SPDX a -> SPDX ()
pack e@(Ref _)  = e
pack e@(Pack _) = e
pack e          = Pack e

instance HasSPDXID (SPDX a) where
    getSPDXID (Ref i)             = i
    getSPDXID (Pack e)            = getSPDXID e

    getSPDXID (Element i _ _)     = i

    getSPDXID (Artifact ie _)     = getSPDXID ie
    getSPDXID (Collection ie _)   = getSPDXID ie
    getSPDXID (Bundle c _)        = getSPDXID c
    getSPDXID (BOM b)             = getSPDXID b
    getSPDXID (SpdxDocument b)    = getSPDXID b
    getSPDXID (Relationship ie _) = getSPDXID ie
    getSPDXID (Annotation ie _)   = getSPDXID ie

    getSPDXID (Package a _)       = getSPDXID a
    getSPDXID (File a _)          = getSPDXID a
    getSPDXID (Snippet a _)       = getSPDXID a
    getSPDXID (SBOM b)            = getSPDXID b

toRef :: SPDX a -> SPDX ()
toRef = Ref . getSPDXID

getType :: SPDX a -> String
getType (Ref _)           = "Ref"
getType (Pack e)          = getType e

getType (Element {})      = "Element"

getType (Artifact {})     = "Artifact"
getType (Collection {})   = "Collection"
getType (Bundle {})       = "Bundle"
getType (BOM {})          = "BOM"
getType (SpdxDocument {}) = "SpdxDocument"
getType (Relationship {}) = "Relationship"
getType (Annotation {})   = "Annotation"

getType (Package {})      = "Package"
getType (File {})         = "File"
getType (Snippet {})      = "Snippet"
getType (SBOM {})         = "SBOM"

getParent :: SPDX a -> Maybe (SPDX ())
getParent (Ref _)             = Nothing

getParent (Pack e)            = getParent e
getParent (Element {})        = Nothing

getParent (Artifact ie _)     = Just $ pack ie
getParent (Collection ie _)   = Just $ pack ie
getParent (Bundle c _)        = Just $ pack c
getParent (BOM b)             = Just $ pack b
getParent (SpdxDocument b)    = Just $ pack b
getParent (Relationship ie _) = Just $ pack ie
getParent (Annotation ie _)   = Just $ pack ie

getParent (Package b _)       = Just $ pack b
getParent (File    b _)       = Just $ pack b
getParent (Snippet b _)       = Just $ pack b
getParent (SBOM b)            = Just $ pack b

instance ToJSON (SPDX a) where
    toJSON (Ref i) = toJSON i
    toJSON e = let
          t = object ["@type" .= getType e]
          mergeObjects :: [Value] -> Value
          mergeObjects list = let
              unObject :: Value -> Object
              unObject (Object o) = o
              unObject _          = undefined -- partial function :see_no_evil:
            in Object (mconcat (map unObject list))

          getJsons :: SPDX a -> [Value]
          getJsons (Ref _)              = undefined
          getJsons (Pack p)             = getJsons p

          getJsons (Element spdxid creationInfo eps) = [ object ["SPDXID" .= spdxid , "creationInfo" .= creationInfo]
                                                       , toJSON  eps]

          getJsons (Artifact p aps)     = toJSON aps : getJsons p
          getJsons (Collection p cps)   = toJSON cps : getJsons p
          getJsons (Bundle p bps)       = toJSON bps : getJsons p
          getJsons (BOM p)              = getJsons p
          getJsons (SpdxDocument p)     = getJsons p
          getJsons (Relationship p rps) = toJSON rps : getJsons p
          getJsons (Annotation p aps)   = toJSON aps : getJsons p

          getJsons (Package p pps)      = toJSON pps : getJsons p
          getJsons (File p fps)         = toJSON fps : getJsons p
          getJsons (Snippet p sps)      = toJSON sps : getJsons p
          getJsons (SBOM p)             = getJsons p

        in mergeObjects $ t : getJsons e

instance FromJSON (SPDX ()) where
    parseJSON (String s) = return $ Ref (T.unpack s)
    parseJSON (Object v) = let
            parseElementJSON :: Object -> Parser (SPDX Element)
            parseElementJSON o = do
                Element <$> o .: "SPDXID"
                          <*> o .: "creationInfo"
                          <*> parseJSON (Object o)
            parseArtifactJSON :: Object -> Parser (SPDX Artifact)
            parseArtifactJSON o = do
                Artifact <$> parseElementJSON o
                        <*> parseJSON (Object o)
            parseCollectionJSON :: Object -> Parser (SPDX Collection)
            parseCollectionJSON o = do
                Collection <$> parseElementJSON o
                        <*> parseJSON (Object o)
            parseBundleJSON :: Object -> Parser (SPDX Bundle)
            parseBundleJSON o = do
                Bundle <$> parseCollectionJSON o
                       <*> parseJSON (Object o)
            parseBOMJSON :: Object -> Parser (SPDX BOM)
            parseBOMJSON o = do
                BOM <$> parseBundleJSON o
            parseSpdxDocumentJSON :: Object -> Parser (SPDX SpdxDocument)
            parseSpdxDocumentJSON o = do
                SpdxDocument <$> parseBundleJSON o
            parseRelationshipJSON :: Object -> Parser (SPDX Relationship)
            parseRelationshipJSON o = do
                Relationship <$> parseElementJSON o
                            <*> parseJSON (Object o)
            parseAnnotationJSON :: Object -> Parser (SPDX Annotation)
            parseAnnotationJSON o = do
                Annotation <$> parseElementJSON o
                           <*> parseJSON (Object o)
            parsePackageJSON :: Object -> Parser (SPDX Package)
            parsePackageJSON o = do
                Package <$> parseArtifactJSON o
                        <*> parseJSON (Object o)
            parseFileJSON :: Object -> Parser (SPDX File)
            parseFileJSON o = do
                File <$> parseArtifactJSON o
                        <*> parseJSON (Object o)
            parseSnippetJSON :: Object -> Parser (SPDX Snippet)
            parseSnippetJSON o = do
                Snippet <$> parseArtifactJSON o
                        <*> parseJSON (Object o)
            parseSBOMJSON :: Object -> Parser (SPDX SBOM)
            parseSBOMJSON o = do
                SBOM <$> parseBOMJSON o
        in ((v .: "@type") :: Parser String) >>= \case
            "Ref"          -> fail "Ref is not an object"

            "Element"      -> pack <$> parseElementJSON v

            "Artifact"     -> pack <$> parseArtifactJSON v
            "Collection"   -> pack <$> parseCollectionJSON v
            "Bundle"       -> pack <$> parseBundleJSON v
            "BOM"          -> pack <$> parseBOMJSON v
            "SpdxDocument" -> pack <$> parseSpdxDocumentJSON v
            "Relationship" -> pack <$> parseRelationshipJSON v
            "Annotation"   -> pack <$> parseAnnotationJSON v

            "Package"      -> pack <$> parsePackageJSON v
            "File"         -> pack <$> parseFileJSON v
            "Snippet"      -> pack <$> parseSnippetJSON v
            "SBOM"         -> pack <$> parseSBOMJSON v
            t              -> fail ("type='" ++ t ++ "' not supported")
    parseJSON _ = fail "not supported type"

setSPDXIDFromContent :: (SPDXID -> SPDX a) -> SPDX a
setSPDXIDFromContent fun = let
      withHole = fun "{}"
      t = getType withHole
      encoded = encode withHole
      hash = md5 encoded
    in fun ("urn:" ++ t ++ ":" ++ show hash)
