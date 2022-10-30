module  SPDX3.Model where

import Data.Text (Text)

data IRI

data CreationInformation
data ExternalMap
data NamespaceMap
class PayloadC a where
    creationInfo :: a -> Maybe CreationInformation
    imports :: a -> [ExternalMap]
    namespaces :: a -> [NamespaceMap]
data Payload
    = Payload
    { _creationInfo :: Maybe CreationInformation
    , _imports :: [ExternalMap]
    , _namespaces :: [NamespaceMap]
    }

class PayloadC a => ElementC a where
    spdxid :: a -> IRI
    name :: a -> Maybe Text
    summary :: a -> Maybe Text

data Actor 

class ElementC a => ArtifactC a where
    originatedBy :: a -> Actor

data SoftwarePurpose
data URL
class ArtifactC a => PackageC a where
    packagePurpose :: a -> SoftwarePurpose
    downloadLocation :: a -> Maybe URL
    homePage :: a -> Maybe URL

data Element
    = E 
    { _spdxid :: IRI
    , _name :: Maybe Text
    , _summaary :: Maybe Text
    }
instance ElementC Element where


data SPDX3 
    = Element !Element
    | Artifact
    | Package