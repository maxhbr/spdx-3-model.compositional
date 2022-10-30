{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
module SPDX3.Model
    where

import Data.Vinyl
import Data.Vinyl.Functor
import Control.Lens hiding (Identity)
import Data.Char
import Data.Singletons.TH (genSingletons)
import Data.Text (Text)
import Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.TH (mkParseJSON)

type IRI = String
type SemVer = String
type ProfileIdentifier = String
type DateTime = String
data DataLicense = CC0
    deriving Show

data CreationInformation'
    = CreationInformation'
    { _specVersion :: SemVer
    , _profile :: [ProfileIdentifier]
    , _created :: DateTime
    , _dataLicense :: DataLicense
    -- , createdBy :: [Rec Attr Actor]
    }
    deriving Show
data ElementProperties'
    = ElementProperties'
    { _name :: Maybe Text
    , _summary :: Maybe Text
    , _description :: Maybe Text
    , _comment :: Maybe Text
    -- , verifiedUsing    
    -- , externalReferences   
    -- , externalIdentifiers
    -- , extensions
    }
    deriving Show
emptyElementProperties = ElementProperties' Nothing Nothing Nothing Nothing

{- ############################################################################
#####  defining fields  #######################################################
############################################################################ -}
data Fields
    = SPDXID
    | CreationInformation
    | ElementProperties
    | OriginatedBy
    | PackagePurpose
    | Elements
    | RelationshipType
    | From
    | To
    | BundleContext -- Context
    deriving Show

{- ############################################################################
#####  defining sets of fields  ###############################################
############################################################################ -}
type Element = [SPDXID, ElementProperties]
type Actor = Element
type Artifact = [OriginatedBy, SPDXID, ElementProperties]
type Package = [PackagePurpose, OriginatedBy, SPDXID, ElementProperties]
type Relationship = [RelationshipType, From, To, SPDXID, ElementProperties]
type Collection = [Elements, SPDXID, ElementProperties]
type Bundle = [BundleContext, Elements, SPDXID, ElementProperties]
type SpdxDocument = Bundle
type BOM = Bundle
type SBOM = BOM

{- ############################################################################
#####  defining SPDX3 data type  ##############################################
############################################################################ -}
data SPDX3
    = Actor !(Rec Attr Actor)
    | Package !(Rec Attr Package)
    | Relationship !(Rec Attr Relationship)
    | Bundle !(Rec Attr Bundle)
    | SpdxDocument !(Rec Attr SpdxDocument)
    | BOM !(Rec Attr BOM)
    | SBOM !(Rec Attr SBOM)
    | SPDX3Ref IRI

{- ############################################################################
#####  defining content of fields  ############################################
############################################################################ -}
type family ElF (f :: Fields) :: * where
  ElF SPDXID = IRI
  ElF CreationInformation = CreationInformation'
  ElF ElementProperties = ElementProperties'
  ElF OriginatedBy = [Rec Attr Actor]
  ElF PackagePurpose = Text -- TODO
  ElF Elements = [SPDX3]
  ElF RelationshipType = String -- TODO
  ElF From = SPDX3
  ElF To = [SPDX3]
  ElF BundleContext = Text

newtype Attr f = Attr { _unAttr :: ElF f }
makeLenses ''Attr

instance Show (Attr SPDXID) where show (Attr x) = "SPDXID: " ++ show x
instance Show (Attr CreationInformation) where show (Attr x) = "creationInformation: " ++ show x
instance Show (Attr ElementProperties) where show (Attr x) = "elementProperties: " ++ show x
instance Show (Attr OriginatedBy) where show (Attr x) = "originatedBy: " ++ show x
instance Show (Attr PackagePurpose) where show (Attr x) = "packagePurpose: " ++ show x
instance Show (Attr Elements) where show (Attr x) = "elements: " ++ show x
instance Show (Attr RelationshipType) where show (Attr x) = "relationshipType: " ++ show x
instance Show (Attr From) where show (Attr x) = "from: " ++ show x
instance Show (Attr To) where show (Attr x) = "to: " ++ show x
instance Show (Attr BundleContext) where show (Attr x) = "context: " ++ show x

{- ############################################################################
#####  Show for SPDX3  ########################################################
############################################################################ -}
deriving instance Show SPDX3

{- ############################################################################
#####  defining construtors  ##################################################
############################################################################ -}
(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

genSingletons [ ''Fields ]

{- ############################################################################
#####  defining getters  ######################################################
############################################################################ -}
getSPDXID :: (SPDXID ∈ fields) => Rec Attr fields -> IRI
getSPDXID = _unAttr <$> (^. rlens @SPDXID)
getElementProperties :: (ElementProperties ∈ fields) => Rec Attr fields -> ElementProperties'
getElementProperties = _unAttr <$> (^. rlens @ElementProperties)
getName :: (ElementProperties ∈ fields) => Rec Attr fields -> Maybe Text
getName = _name . getElementProperties
getSummary :: (ElementProperties ∈ fields) => Rec Attr fields -> Maybe Text
getSummary = _summary . getElementProperties
getElements :: (Elements ∈ fields) => Rec Attr fields -> [SPDX3]
getElements = _unAttr <$> (^. rlens @Elements)

{- ############################################################################
#####  defining helpers  ######################################################
############################################################################ -}
-- unpack :: SPDX3 -> Rec Attr fields
unpack (Actor attrs) = rcast attrs
unpack (Package attrs) = rcast attrs
unpack (Relationship attrs) = rcast attrs
unpack (Bundle attrs) = rcast attrs
unpack (SpdxDocument attrs) = rcast attrs
unpack (BOM attrs) = rcast attrs
unpack (SBOM attrs) = rcast attrs
unpack (SPDX3Ref iri) = rcast $ (SSPDXID =:: iri) :& RNil

addElements :: (Elements ∈ fields) => [SPDX3] -> Rec Attr fields -> Rec Attr fields
addElements es attrs = let
      oldEs = getElements attrs
    in (rput $ SElements =:: (es ++ oldEs)) attrs

toRef :: SPDX3 -> SPDX3
toRef r@(SPDX3Ref _) = r
toRef (Actor attrs) = (SPDX3Ref . getSPDXID) attrs
toRef (Package attrs) = (SPDX3Ref . getSPDXID) attrs
toRef (Relationship attrs) = (SPDX3Ref . getSPDXID) attrs
toRef (Bundle attrs) = (SPDX3Ref . getSPDXID) attrs
toRef (SpdxDocument attrs) = (SPDX3Ref . getSPDXID) attrs
toRef (BOM attrs) = (SPDX3Ref . getSPDXID) attrs
toRef (SBOM attrs) = (SPDX3Ref . getSPDXID) attrs

{- ############################################################################
#####  defining builders  #####################################################
############################################################################ -}
mkElement :: String -> Rec Attr Element
mkElement spdxid = (SSPDXID =:: spdxid)
                 :& (SElementProperties =:: emptyElementProperties)
                 :& RNil

mkArtifact :: Rec Attr Element -> [Rec Attr Actor] -> Rec Attr Artifact
mkArtifact element originatedBy = (SOriginatedBy =:: originatedBy)
                                :& element

mkPackage :: Rec Attr Artifact -> Text -> Rec Attr Package
mkPackage artifact packagePurpose = (SPackagePurpose =:: packagePurpose)
                                  :& artifact

mkRelationship :: Rec Attr Element -> String -> SPDX3 -> [SPDX3] -> Rec Attr Relationship
mkRelationship element relationshipType from to = (SRelationshipType =:: relationshipType)
                                                :& (SFrom =:: from)
                                                :& (STo =:: to)
                                                :& element

mkCollection :: Rec Attr Element -> [SPDX3] -> Rec Attr Collection
mkCollection element elements = (SElements =:: elements) :& element
mkBundle :: Rec Attr Collection -> Text -> Rec Attr Bundle
mkBundle collection context = (SBundleContext =:: context) :& collection 
mkBOM :: Rec Attr Bundle -> Rec Attr BOM
mkBOM = id
mkSBOM :: Rec Attr Bundle -> Rec Attr SBOM
mkSBOM = id