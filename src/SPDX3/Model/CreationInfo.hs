{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module SPDX3.Model.CreationInfo where
import           Data.Aeson
import           Data.Aeson.Encoding      (string)
import           Data.Time.Clock
import           Data.Time.Format.ISO8601
import           GHC.Generics             (Generic)

data ActorType = PERSON | ORGANIZATION | TOOL
    deriving (Generic, Eq, Show)
instance ToJSON ActorType where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ActorType
data Actor where
    Actor :: Maybe String -> Maybe ActorType -> Actor
    deriving (Generic, Eq, Show)
instance ToJSON Actor where
  toJSON (Actor name actorType) =
    object ["name" .= name, "actorType" .= actorType]
instance FromJSON Actor where
  parseJSON = withObject "Actor" $ \o -> do
    Actor <$> o .: "name" <*> o .: "actorType"

type SemVer = String
type ProfileIdentifier = String
data DataLicense = CC0
  deriving (Generic, Eq, Show)
instance ToJSON DataLicense where
  toJSON CC0 = "CC0"
instance FromJSON DataLicense where
  parseJSON _ = return CC0
data CreationInfo = CreationInfo
  { _specVer     :: SemVer
  , _profile     :: [ProfileIdentifier]
  , _created     :: UTCTime
  , _dataLicense :: DataLicense
  , _createdBy   :: [Actor]
  }
  deriving (Generic, Eq, Show)
instance ToJSON CreationInfo where
  toJSON c = object
    [ "specVer" .= _specVer c
    , "profile" .= _profile c
    , "created" .= iso8601Show (_created c)
    , "dataLicense" .= _dataLicense c
    , "createdBy" .= _createdBy c
    ]
instance FromJSON  CreationInfo where
  parseJSON = withObject "CreationInfo" $ \o -> do
    created <- iso8601ParseM <$> o .:  "created"
    CreationInfo
        <$> o
        .:  "specVer"
        <*> o
        .:  "profile"
        <*> created
        <*> o
        .:  "dataLicense"
        <*> o
        .:  "createdBy"

mkCreationInfo :: [Actor] -> UTCTime -> CreationInfo
mkCreationInfo actors created =
  CreationInfo "3.0.0" ["core", "software", "licensing"] created CC0 actors

mkCreationInfoIO :: [Actor] -> IO CreationInfo
mkCreationInfoIO []     = fail "Actors are required"
mkCreationInfoIO actors = mkCreationInfo actors <$> getCurrentTime

class HasCreationInfo a where
    getCreationInfo :: a -> CreationInfo
