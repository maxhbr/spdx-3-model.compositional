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

-- -- ############################################################################
-- -- ##  Element  ###############################################################
-- -- ############################################################################

data ElementProperties
    = ElementProperties
    { _epName :: Maybe String
    , _epSummary :: Maybe String
    , _epDescription :: Maybe String
    , _epComment :: Maybe String
    } deriving (Show)
emptyEps :: ElementProperties
emptyEps = ElementProperties Nothing Nothing Nothing Nothing
epsFromName :: String -> ElementProperties
epsFromName name = ElementProperties (Just name) Nothing Nothing Nothing
instance ToJSON ElementProperties where
    toJSON (ElementProperties name summary description comment) =
        object [ "name" .= name
               , "summary" .= summary
               , "description" .= description
               , "comment" .= comment
               ]
instance FromJSON ElementProperties where
    parseJSON = withObject "ElementProperties" $ \o -> do
        ElementProperties <$> o .:? "name"
                          <*> o .:? "summary"
                          <*> o .:? "description"
                          <*> o .:? "comment"

data Element

data Artifact
data Collection
data Bundle
data BOM
data Relationship
data Annotation

-- Software
data Package
data File
data Snippet
data SBOM

data SPDX a where
    Ref          :: SPDXID -> SPDX ()
    Pack         :: SPDX a -> SPDX ()

    Element      :: SPDXID -> CreationInfo -> ElementProperties -> SPDX Element

    Artifact     :: SPDX Element -> SPDX Artifact
    Collection   :: SPDX Element -> [SPDX ()] -> SPDX Collection
    Bundle       :: SPDX Collection -> SPDX Bundle
    BOM          :: SPDX Bundle -> SPDX BOM
    Relationship :: SPDX Element -> RelationshipType -> SPDX () -> [SPDX ()] -> SPDX Relationship
    Annotation   :: SPDX Element -> String -> SPDX () -> SPDX Annotation

    -- Software
    Package      :: SPDX Artifact -> SPDX Package
    File         :: SPDX Artifact -> SPDX File
    Snippet      :: SPDX Artifact -> SPDX Snippet
    SBOM         :: SPDX BOM -> SPDX SBOM

pack :: SPDX a -> SPDX ()
pack e@(Ref _) = e
pack e@(Pack _) = e
pack e = Pack e
deriving instance Show (SPDX a)
instance HasSPDXID (SPDX a) where
    getSPDXID (Ref i) = i
    getSPDXID (Pack e) = getSPDXID e

    getSPDXID (Element i _ _) = i

    getSPDXID (Artifact ie) = getSPDXID ie
    getSPDXID (Collection ie _) = getSPDXID ie
    getSPDXID (Bundle c) = getSPDXID c
    getSPDXID (BOM b) = getSPDXID b
    getSPDXID (Relationship ie _ _ _) = getSPDXID ie
    getSPDXID (Annotation ie _ _) = getSPDXID ie

    getSPDXID (Package a) = getSPDXID a
    getSPDXID (File a) = getSPDXID a
    getSPDXID (Snippet a) = getSPDXID a
    getSPDXID (SBOM b) = getSPDXID b


getElements :: SPDX a -> [SPDX ()]
getElements e = let
        getImplicitElements' :: SPDX a -> [SPDX ()]
        getImplicitElements' (Pack e) = getImplicitElements e
        getImplicitElements' e@(Collection _ es) = concatMap getImplicitElements es
        getImplicitElements' e@(Relationship _ _ from to) = concatMap getImplicitElements (from:to)
        getImplicitElements' e@(Annotation _ _ subject) = concatMap getImplicitElements [subject]
        getImplicitElements' _ = []
        getImplicitElements :: forall a. SPDX a -> [SPDX ()]
        getImplicitElements e = getImplicitElements' e ++ maybe [] getImplicitElements (getParent e)
    in pack e : getImplicitElements e

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
getParent (Ref _)                 = Nothing

getParent (Pack e)                = getParent e
getParent (Element {})            = Nothing

getParent (Artifact ie)           = Just $ pack ie
getParent (Collection ie _)       = Just $ pack ie
getParent (Bundle c)              = Just $ pack c
getParent (BOM b)                 = Just $ pack b
getParent (Relationship ie _ _ _) = Just $ pack ie
getParent (Annotation ie _ _)     = Just $ pack ie

getParent (Package b)            = Just $ pack b
getParent (File    b)            = Just $ pack b
getParent (Snippet b)            = Just $ pack b
getParent (SBOM b)               = Just $ pack b

getJsonKMs :: KeyValue kv => SPDX a -> [kv]
getJsonKMs (Ref i) = undefined
getJsonKMs (Pack e) = getJsonKMs e

getJsonKMs (Element i ci eps) = [ "SPDXID" .= i
                                , "creationInfo" .= ci
                                ]

getJsonKMs (Artifact _) =  []
getJsonKMs (Collection _ elements) = [ "elements" .= elements]
getJsonKMs (Bundle _) = []
getJsonKMs (BOM _) = []
getJsonKMs (Relationship _ t from to) = [ "relationshipType" .= t
                                        , "from" .= from
                                        , "to" .= to
                                        ]
getJsonKMs (Annotation _ statement subject) = [ "statement" .= statement
                                              , "subject" .= subject
                                              ]

getJsonKMs (Package _) = []
getJsonKMs (File _) = []
getJsonKMs (Snippet _) = []
getJsonKMs (SBOM _) = []

instance ToJSON (SPDX a) where
    toJSON (Ref i) = toJSON i
    toJSON e = let
          t = getType e
          getAllJsonKMs :: KeyValue kv => SPDX a -> [kv]
          getAllJsonKMs e = getJsonKMs e ++ (case getParent e of
              Just parent -> getAllJsonKMs parent
              _ -> []
              )
        in object $ ("@type" .= t) : getAllJsonKMs e

parseElementJSON :: Value -> Parser (SPDX Element)
parseElementJSON = withObject "Element" $ \o -> do
    Element <$> o .: "SPDXID"
            <*> o .: "creationInfo"
            <*> parseJSON (Object o)
parseArtifactJSON :: Value -> Parser (SPDX Artifact)
parseArtifactJSON = withObject "Artifact" $ \o -> do
    Artifact <$> parseElementJSON (Object o)
parseCollectionJSON :: Value -> Parser (SPDX Collection)
parseCollectionJSON = withObject "Collection" $ \o -> do
    Collection <$> parseElementJSON (Object o)
               <*> o .: "elements"
parseRelationshipJSON :: Value -> Parser (SPDX Relationship)
parseRelationshipJSON = withObject "Relationship" $ \o -> do
    Relationship <$> parseElementJSON (Object o)
                 <*> o .: "relationshipType"
                 <*> o .: "from"
                 <*> o .: "to"
parseAnnotationJSON :: Value -> Parser (SPDX Annotation)
parseAnnotationJSON = withObject "Annotation" $ \o -> do
    Annotation <$> parseElementJSON (Object o)
               <*> o .: "statement"
               <*> o .: "subject"
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
    let ie = Element i ci (epsFromName name)
    return (Artifact ie)
artifact :: SPDXID -> String -> SPDX_M (SPDX ())
artifact i name = pack <$> artifact' i name

collection' :: SPDXID -> SPDX_M [SPDX ()] -> SPDX_M (SPDX Collection)
collection' i es = do
    ci <- ask
    let ie = Element i ci emptyEps
    Collection ie <$> es
collection :: SPDXID -> SPDX_M [SPDX ()] -> SPDX_M (SPDX ())
collection i es = pack <$> collection' i es

annotation' :: SPDXID -> String -> SPDX_M (SPDX ()) -> SPDX_M (SPDX Annotation)
annotation' i stmnt subject = do
    ci <- ask
    let ie = Element i ci emptyEps
    Annotation ie stmnt <$> subject
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
