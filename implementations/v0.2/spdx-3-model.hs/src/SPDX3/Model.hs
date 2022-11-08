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



-- ############################################################################
-- ##  ElementTrait  ##########################################################
-- ############################################################################

class (ToJSON a, Typeable a, Show a) => ElementTrait a where
    getTraitName :: a -> String
    getTypeRep :: a -> TypeRep
    getTypeRep = typeOf

data ElementTraitP a where
    ElementTraitP :: (Generic b, ElementTrait b) => b -> ElementTraitP a
instance ToJSON (ElementTraitP a) where
    toJSON (ElementTraitP a) = toJSON a
deriving instance Show (ElementTraitP a)
instance Typeable a => ElementTrait (ElementTraitP a) where
    getTraitName (ElementTraitP a) = getTraitName a
    getTypeRep (ElementTraitP a) = getTypeRep a
type ElementTraitsMap a = Map.HashMap TypeRep (ElementTraitP a)

addTrait :: (ElementTrait b, Generic b) => b -> ElementTraitsMap a -> ElementTraitsMap a
addTrait t = Map.insert (getTypeRep t) (ElementTraitP t) 

-- ############################################################################
-- ##  ElementClass  ##########################################################
-- ############################################################################

class (Show a, ToJSON a) => ElementClass a where
    getClassName :: a -> String
    getClassElements :: a -> [Element ()]

-- ############################################################################
-- ##  Element  ###############################################################
-- ############################################################################

data Element a where
    Element :: (ElementClass a, Typeable a) => SPDXID -> CreationInfo -> Maybe String -> a -> ElementTraitsMap a -> Element a
    Ref :: SPDXID -> Element ()
    Packed :: ElementClass a => Element a -> Element ()
deriving instance (Show (Element a))
deriving instance (Typeable (Element a))
instance HasSPDXID (Element a) where
    getSPDXID (Element i _ _ _ _) = i
    getSPDXID (Ref i) = i
    getSPDXID (Packed element) = getSPDXID element
instance ToJSON (Element a) where
    toJSON e@(Element spdxid creationInfo name a traitsMap) = let
        traits = Map.elems traitsMap
        mergeObjects :: [Value] -> Value
        mergeObjects list = let
            unObject :: Value -> Object
            unObject (Object o) = o
            unObject _ = undefined -- partial function :see_no_evil:
          in Object (mconcat (map unObject list))
        baseObject = object [ "SPDXID" .= spdxid
                            , "creationInfo" .= creationInfo
                            , "name" .= name
                            , "class" .= getClassName a
                            , "traits" .= map getTraitName traits
                            ]
        aJSON = toJSON a
        traitJSONS = map toJSON traits
        in mergeObjects (baseObject:aJSON:traitJSONS)
    toJSON e@(Ref spdxid) = object ["SPDXID" .= spdxid]
    toJSON (Packed e) = toJSON e

pack :: ElementClass a => Element a -> Element ()
pack e@(Ref _) = e
pack e@(Packed _) = e
pack e = Packed e

toRef :: Element a -> Element ()
toRef = Ref . getSPDXID

getName :: Element a -> Maybe String
getName (Element _ _ n _ _) = n
getName (Packed e) = getName e
getName _ = Nothing

getElements :: ElementClass a =>  Element a -> [Element ()]
getElements e@(Element _ _ _ c _) = pack e:getClassElements c
getElements e = [pack e]

-- ############################################################################
-- ##  ElementClasses  ########################################################
-- ############################################################################

data Artifact = Artifact
    deriving (Show)
instance ToJSON Artifact where
    toJSON _ = object []
instance ElementClass Artifact where
    getClassName _ = "Artifact"
    getClassElements a = []

data Relationship = Relationship RelationshipType (Element ()) [Element ()]
    deriving (Show)
instance ToJSON Relationship where
    toJSON (Relationship t from to) =
        object [ "relationshipType" .= toJSON t
               , "from" .= toJSON from
               , "to" .= toJSON to
               ]
instance ElementClass Relationship where
    getClassName _ = "Relationship"
    getClassElements a@(Relationship _ from to) = from:to

data Collection = Collection [Element ()] [SPDXID]
    deriving (Show)
instance ToJSON Collection where
    toJSON (Collection elems rootElems) =
        object [ "elemes" .= toJSON elems
               , "rootElems" .= toJSON rootElems
               ]
instance ElementClass Collection where
    getClassName _ = "Collection"
    getClassElements a@(Collection es _) = es

-- ############################################################################
-- ############################################################################
-- ############################################################################

data SoftwareArtifact
    = File
    | Package (Maybe String) (Maybe String)
    | Snippet
    deriving (Show,Generic)
instance ToJSON SoftwareArtifact where
    toJSON sa = let
          details File = []
          details (Package homepageUrl downloadUrl) =
              [ "softwareHomepagedUrl" .= homepageUrl
              , "softwarePackageDownloadUrl" .= downloadUrl
              ]
          details Snippet = []
          sat :: SoftwareArtifact -> String
          sat File = "File"
          sat (Package _ _) = "Package"
          sat Snippet = "Snippet"
        in object  (("softwareArtifactType" .= sat sa) : details sa)
instance ElementTrait SoftwareArtifact where
  getTraitName _ = "SoftwareArtifact"

-- ############################################################################
-- ##  example  ###############################################################
-- ############################################################################

mkExample :: Element Collection
mkExample = let
        e0 = Element "urn:spdx:Element0" mkCreationInfo (Just "Element0") (Artifact) (addTrait (Package (Just "https://someDomain.invalid/download.tar.gz") Nothing) mempty)
        e1 = Element "urn:spdx:Element1" mkCreationInfo (Just "Element1") (Artifact) mempty
        e2 = Element "urn:spdx:Element2" mkCreationInfo (Just "Element2") (Artifact) (addTrait File mempty)
        r0 = Element "urn:spdx:Rel" mkCreationInfo Nothing (Relationship CONTAINS (pack e0) [pack e1, toRef e2]) mempty
        c = Element "urn:spdx:Document" mkCreationInfo (Just "Super SPDX Document") (Collection [pack e0, pack r0, pack e2] [getSPDXID e0]) mempty
    in c