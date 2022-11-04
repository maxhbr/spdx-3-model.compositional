{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module SPDX3.Model
    where
import GHC.Generics (Generic)
import Data.Aeson
import SPDX3.SPDXID
import SPDX3.CreationInfo
import SPDX3.RelationshipType
import Data.Data
import Data.Dynamic
import qualified Data.HashMap.Strict as Map

class (ToJSON a, Typeable a) => Elementic a where
    getType :: a -> String
    getTypeRep :: a -> TypeRep
    getTypeRep = typeOf
data ElementContent where
    ElementContent :: (Generic a, Show a, Elementic a) => a -> ElementContent
instance ToJSON ElementContent where
    toJSON (ElementContent a) = toJSON a
instance Elementic ElementContent where
    getType (ElementContent a) = getType a
    getTypeRep (ElementContent a) = getTypeRep a
deriving instance Show ElementContent
type ElementContentMap = Map.HashMap TypeRep ElementContent
data ElementBody where
    Artifact :: ElementBody
    Relation :: RelationshipType -> SPDXID -> [SPDXID] -> ElementBody
    Collection :: [SPDXID] -> ElementBody
deriving instance Show ElementBody
data Element where
  Element :: { _SPDXID :: SPDXID
             , _creationInfo :: CreationInfo
             , _name :: Maybe String
             , _content :: ElementContentMap
             , _body :: ElementBody
             }
          -> Element
deriving instance Show Element
instance HasSPDXID Element where
    getSPDXID :: Element -> SPDXID
    getSPDXID = _SPDXID
instance ToJSON Element where
    toJSON e = let
        contents = Map.elems (_content e)
        mergeObjects :: Value -> [Value] -> Value
        mergeObjects init tail = let
            unObject :: Value -> Object
            unObject (Object o) = o
            unObject _ = undefined -- partial function :see_no_evil:
          in Object (mconcat (unObject init : map unObject tail))
        baseObject = object [ "SPDXID" .= _SPDXID e
                            , "creationInfo" .= _creationInfo e
                            , "name" .= _name e
                            , "profiles" .= map getType contents
                            ]
        contentObjects = map toJSON contents
        in mergeObjects baseObject contentObjects

addContent :: Element -> ElementContent -> Element
addContent e@Element{_content = content} ec = e{_content = Map.insert (getTypeRep ec) ec content}

data ElementCore where
    ElementCore :: { _summary :: Maybe String
                   , _description :: Maybe String
                   , _comment :: Maybe String
                   -- , _verifiedUsing
                   -- , _externalReferences
                   -- , _externalIdentifiers
                   -- , _extensions
                   } -> ElementCore
    deriving (Generic, Eq, Show)
instance ToJSON ElementCore where
    toEncoding = genericToEncoding defaultOptions
instance Elementic ElementCore where
    getType _ = "core"

-- instance ToJSON ElementMetadata where
--     toEncoding = genericToEncoding defaultOptions
-- class HasMetadata a where
--     getMetadata :: a -> ElementMetadata

-- -- mkElementMetadata :: SPDXID -> Maybe String -> ElementMetadata
-- -- mkElementMetadata id name = ElementMetadata id mkCreationInfo name Nothing

-- -- {- ######################################################################### -}
-- -- data Artifact = MkArtifact
-- --   { _artifactMetadata :: ElementMetadata
-- --   , _originatedBy :: [SPDXID]
-- --   }
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON Artifact where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- instance HasMetadata Artifact where
-- --     getMetadata (MkArtifact em _) = em
-- -- instance HasSPDXID Artifact where
-- --     getSPDXID = getSPDXID . getMetadata

-- -- {- ######################################################################### -}
-- -- data CollectionType = SpdxDocument | BOM
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON CollectionType where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- data Collection = MkCollection
-- --   { _collectionMetadata :: ElementMetadata
-- --   , _collectionType :: Maybe CollectionType
-- -- --   , _imports ::
-- --   , _collectionElements :: [ElementRef]
-- --   , _collectionRootElements :: [SPDXID]
-- --   , _collectionContext :: Maybe String
-- --   }
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON Collection where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- instance HasMetadata Collection where
-- --     getMetadata (MkCollection em _ _ _ _) = em
-- -- instance HasSPDXID Collection where
-- --     getSPDXID = getSPDXID . getMetadata

-- -- {- ######################################################################### -}
-- -- data RelationshipCompleteness = RelationshipComplete | RelationshipIncomplete
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON RelationshipCompleteness where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- data Relationship = MkRelationship
-- --   { _relationshipMetadata :: ElementMetadata
-- --   , _relationshipType :: RelationshipType
-- --   , _relationshipCompleteness :: Maybe RelationshipCompleteness
-- --   , _relationshipFrom :: ElementRef
-- --   , _relationshipTo :: [ElementRef]
-- --   }
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON Relationship where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- instance HasMetadata Relationship where
-- --     getMetadata (MkRelationship em _ _ _ _) = em
-- -- instance HasSPDXID Relationship where
-- --     getSPDXID = getSPDXID . getMetadata

-- -- {- ######################################################################### -}
-- -- data IdentityType = IdentityTypePerson | IdentityTypeOrganization | IdentityTypeTool
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON IdentityType where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- data Identity = MkIdentity
-- --   { _identityMetadata :: ElementMetadata
-- --   , _identityType :: IdentityType
-- --   }
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON Identity where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- instance HasMetadata Identity where
-- --     getMetadata (MkIdentity em _) = em
-- -- instance HasSPDXID Identity where
-- --     getSPDXID = getSPDXID . getMetadata

-- -- {- ######################################################################### -}

-- -- data ElementRef
-- --     = ElementRef SPDXID
-- --     | InlineElement Element
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON ElementRef where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- toRef :: HasSPDXID a => a -> ElementRef
-- -- toRef = ElementRef . getSPDXID

-- -- data Element
-- --     = Artifact !Artifact
-- --     | Collection !Collection
-- --     | Relationship !Relationship
-- --     | Identity !Identity
-- --     deriving (Generic, Eq, Show)
-- -- instance ToJSON Element where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- instance HasSPDXID Element where
-- --     getSPDXID (Artifact o) = getSPDXID o
-- --     getSPDXID (Collection o) = getSPDXID o
-- --     getSPDXID (Relationship o) = getSPDXID o
-- --     getSPDXID (Identity o) = getSPDXID o
-- -- packArtifact = InlineElement . Artifact
-- -- packCollection = InlineElement . Collection
-- -- packRelationship = InlineElement . Relationship
-- -- packIdentity = InlineElement . Identity

-- -- unRefElements :: [ElementRef] -> [Element]
-- -- unRefElements [] = []
-- -- unRefElements (ElementRef _:ers) = unRefElements ers
-- -- unRefElements (InlineElement e:ers) = e: unRefElements ers
-- -- elements :: Element -> [Element]
-- -- elements e@(Collection MkCollection{_collectionElements = ers}) = e:concatMap elements (unRefElements ers)
-- -- elements e@(Relationship MkRelationship{_relationshipFrom = er, _relationshipTo = ers}) = e:unRefElements [er] ++ concatMap elements (unRefElements ers)
-- -- elements e = [e]

mkExample :: Element
mkExample = let
        e1 = Element "urn:spdx:Document" mkCreationInfo (Just "Super SPDX Document") mempty Artifact
        e1' = addContent e1 (ElementContent (ElementCore (Just "Summary") (Just "Description") Nothing))
    in e1'
-- -- mkExample = let
-- --     artifact1 = MkArtifact (mkElementMetadata "urn:spdx:Artifact1" (Just "Artifact 1")) []
-- --     artifact2 = MkArtifact (mkElementMetadata "urn:spdx:Artifact2" (Just "Artifact 2")) []
-- --     artifact3 = MkArtifact (mkElementMetadata "urn:spdx:Artifact3" (Just "Artifact 3")) []
-- --     relationship = MkRelationship (mkElementMetadata "urn:spdx:Relationship" (Just "Relationship")) CONTAINS (Just RelationshipComplete) (toRef artifact1) [toRef artifact2, packArtifact artifact3]
-- --     collection = MkCollection (mkElementMetadata "urn:spdx:SBOM" (Just "SBOM")) Nothing [packArtifact artifact1, packArtifact artifact2, packRelationship relationship] [] Nothing
-- --     in Collection collection