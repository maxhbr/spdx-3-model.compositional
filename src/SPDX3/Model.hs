{-# LANGUAGE DeriveGeneric #-}

module SPDX3.Model
    where
import GHC.Generics (Generic)
import Data.Aeson
import SPDX3.RelationshipType

type SPDXID = String
class HasSPDXID a where
    getSPDXID :: a -> SPDXID
    toRef :: a -> Element
    toRef = ElementRef . getSPDXID
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
    , createdBy :: [SPDXID]
    }
    deriving (Generic, Eq, Show)
instance ToJSON CreationInfo where
    toEncoding = genericToEncoding defaultOptions
data ElementMetadata
    = ElementMetadata
    { _SPDXID :: SPDXID
    , _creationInfo :: CreationInfo
    , _name :: Maybe String
    , _summary :: Maybe String
    }
    deriving (Generic, Eq, Show)
instance ToJSON ElementMetadata where
    toEncoding = genericToEncoding defaultOptions
instance HasSPDXID ElementMetadata where
    getSPDXID = _SPDXID
class HasMetadata a where
    getMetadata :: a -> ElementMetadata

mkElementMetadata :: SPDXID -> Maybe String -> ElementMetadata
mkElementMetadata id name = let
    creationInfo = CreationInfo "3.0.0" ["core"] "$UTC" CC0 []
    in ElementMetadata id creationInfo name Nothing

{- ######################################################################### -}
newtype Actor = MkActor ElementMetadata
    deriving (Generic, Eq, Show)
instance ToJSON Actor where
    toEncoding = genericToEncoding defaultOptions
instance HasMetadata Actor where
    getMetadata (MkActor em) = em
instance HasSPDXID Actor where
    getSPDXID = getSPDXID . getMetadata

{- ######################################################################### -}
data Artifact = MkArtifact
  { _artifactMetadata :: ElementMetadata
  , _originatedBy :: [Actor]
  } 
    deriving (Generic, Eq, Show)
instance ToJSON Artifact where
    toEncoding = genericToEncoding defaultOptions
instance HasMetadata Artifact where
    getMetadata (MkArtifact em _) = em
instance HasSPDXID Artifact where
    getSPDXID = getSPDXID . getMetadata

{- ######################################################################### -}
data CollectionType = SpdxDocument | BOM
    deriving (Generic, Eq, Show)
instance ToJSON CollectionType where
    toEncoding = genericToEncoding defaultOptions
data Collection = MkCollection
  { _collectionMetadata :: ElementMetadata
  , _collectionType :: Maybe CollectionType
--   , _imports ::
  , _collectionElements :: [Element]
  , _collectionRootElements :: [SPDXID]
  , _collectionContext :: Maybe String
  }
    deriving (Generic, Eq, Show)
instance ToJSON Collection where
    toEncoding = genericToEncoding defaultOptions
instance HasMetadata Collection where
    getMetadata (MkCollection em _ _ _ _) = em
instance HasSPDXID Collection where
    getSPDXID = getSPDXID . getMetadata

{- ######################################################################### -}
data RelationshipCompleteness = RelationshipComplete | RelationshipIncomplete
    deriving (Generic, Eq, Show)
instance ToJSON RelationshipCompleteness where
    toEncoding = genericToEncoding defaultOptions
data Relationship = MkRelationship
  { _relationshipMetadata :: ElementMetadata
  , _relationshipType :: RelationshipType
  , _relationshipCompleteness :: Maybe RelationshipCompleteness
  , _relationshipFrom :: Element
  , _relationshipTo :: [Element]
  }
    deriving (Generic, Eq, Show)
instance ToJSON Relationship where
    toEncoding = genericToEncoding defaultOptions
instance HasMetadata Relationship where
    getMetadata (MkRelationship em _ _ _ _) = em
instance HasSPDXID Relationship where
    getSPDXID = getSPDXID . getMetadata

{- ######################################################################### -}
data IdentityType = IdentityTypePerson | IdentityTypeOrganization | IdentityTypeTool
    deriving (Generic, Eq, Show)
instance ToJSON IdentityType where
    toEncoding = genericToEncoding defaultOptions
data Identity = MkIdentity
  { _identityMetadata :: ElementMetadata
  , _identityType :: IdentityType
  }
    deriving (Generic, Eq, Show)
instance ToJSON Identity where
    toEncoding = genericToEncoding defaultOptions
instance HasMetadata Identity where
    getMetadata (MkIdentity em _) = em
instance HasSPDXID Identity where
    getSPDXID = getSPDXID . getMetadata

{- ######################################################################### -}

data Element
    = ElementRef SPDXID
    | Artifact !Artifact
    | Collection !Collection
    | Relationship !Relationship
    | Identity !Identity
    | Actor !Actor
    deriving (Generic, Eq, Show)
instance ToJSON Element where
    toEncoding = genericToEncoding defaultOptions
instance HasSPDXID Element where
    getSPDXID (ElementRef id) = id
    getSPDXID (Artifact o) = getSPDXID o
    getSPDXID (Collection o) = getSPDXID o
    getSPDXID (Relationship o) = getSPDXID o
    getSPDXID (Identity o) = getSPDXID o
    getSPDXID (Actor o) = getSPDXID o
elements :: Element -> [Element]
elements e@(Collection MkCollection{_collectionElements = es}) = e:concatMap elements es
elements e@(Relationship MkRelationship{_relationshipFrom = e', _relationshipTo = es}) = e:e':concatMap elements es
elements e = [e]

mkExample :: Element
mkExample = let
    actor = MkActor (mkElementMetadata "urn:spdx:Actor" (Just "Some Actor"))
    artifact1 = MkArtifact (mkElementMetadata "urn:spdx:Artifact1" (Just "Artifact 1")) [actor]
    artifact2 = MkArtifact (mkElementMetadata "urn:spdx:Artifact2" (Just "Artifact 2")) [actor]
    artifact3 = MkArtifact (mkElementMetadata "urn:spdx:Artifact3" (Just "Artifact 3")) [actor]
    relationship = MkRelationship (mkElementMetadata "urn:spdx:Relationship" (Just "Relationship")) CONTAINS (Just RelationshipComplete) (toRef artifact1) [toRef artifact2, Artifact artifact3]
    collection = MkCollection (mkElementMetadata "urn:spdx:SBOM" (Just "SBOM")) Nothing [Artifact artifact1, Artifact artifact2, Relationship relationship] [] Nothing
    in Collection collection