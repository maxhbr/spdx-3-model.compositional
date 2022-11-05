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

-- ############################################################################
-- ############################################################################
-- ############################################################################

data ElementContent where
    ElementContent :: (Generic a, Show a, Elementic a) => a -> ElementContent
instance ToJSON ElementContent where
    toJSON (ElementContent a) = toJSON a
instance Elementic ElementContent where
    getType (ElementContent a) = getType a
    getTypeRep (ElementContent a) = getTypeRep a
deriving instance Show ElementContent
type ElementContentMap = Map.HashMap TypeRep ElementContent

-- ############################################################################
-- ############################################################################
-- ############################################################################

class (Show a, ToJSON a) => ElementBody a where
    getClass :: a -> String

data Artifact
    = Artifact
    deriving (Generic, Show)
instance ToJSON Artifact where
    toJSON _ = object []
instance ElementBody Artifact where
    getClass _ = "Artifact"

data Collection
    = Collection
    { _elements :: [Element]
    , _rootElements :: [SPDXID]
    }
    deriving (Generic, Show)
instance ToJSON Collection where
    toEncoding = genericToEncoding defaultOptions
instance ElementBody Collection where
    getClass _ = "Collection"

data Relationship
    = Relationship
    { _relationshipType :: RelationshipType
    , _from :: Element
    , _to :: [Element]
    }
    deriving (Generic, Show)
instance ToJSON Relationship where
    toEncoding = genericToEncoding defaultOptions
instance ElementBody Relationship where
    getClass _ = "Relationship"

-- ############################################################################
-- ############################################################################
-- ############################################################################
    
data Element where
  Element :: ElementBody a
          => { _SPDXID :: SPDXID
             , _creationInfo :: CreationInfo
             , _name :: Maybe String
             , _content :: ElementContentMap
             , _body :: a
             }
          -> Element
  ElementRef :: SPDXID -> Element
deriving instance Show Element
instance HasSPDXID Element where
    getSPDXID :: Element -> SPDXID
    getSPDXID = _SPDXID
instance ToJSON Element where
    toJSON e@Element{_body = body} = let
        contents = Map.elems (_content e)
        mergeObjects :: [Value] -> Value
        mergeObjects list = let
            unObject :: Value -> Object
            unObject (Object o) = o
            unObject _ = undefined -- partial function :see_no_evil:
          in Object (mconcat (map unObject list))
        baseObject = object [ "SPDXID" .= _SPDXID e
                            , "creationInfo" .= _creationInfo e
                            , "name" .= _name e
                            , "profiles" .= map getType contents
                            , "class" .= getClass body
                            ]
        bodyJSON = toJSON body
        contentObjects = map toJSON contents
        in mergeObjects (baseObject : bodyJSON: contentObjects)
    toJSON e@(ElementRef spdxid) = object ["SPDXID" .= spdxid]
toRef :: Element -> Element
toRef e@Element{_SPDXID = spdxid} = ElementRef spdxid
toRef e = e

addContent :: Element -> ElementContent -> Element
addContent e@Element{_content = content} ec = e{_content = Map.insert (getTypeRep ec) ec content}
addContent _ _ = undefined -- partial function :see_no_evil:

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

mkExample :: Element
mkExample = let
        c1 = Element "urn:spdx:Document" mkCreationInfo (Just "Super SPDX Document") mempty (Collection [ElementRef "urn:spdx:someRef"] [])
        c1' = addContent c1 (ElementContent (ElementCore (Just "Summary") (Just "Description") Nothing))
    in c1'