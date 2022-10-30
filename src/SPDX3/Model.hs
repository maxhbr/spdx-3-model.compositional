{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module  SPDX3.Model
    where

import Data.Text (Text)
import Data.Aeson as A
import qualified Data.Aeson.Types as A

type IRI = Text

class Aesonic a where
    toObject :: a -> A.Object

    parseForKey :: (FromJSON b) => a -> A.Key -> A.Parser b
    parseForKey a k = toObject a A..: k
    getForKey :: (FromJSON b) => a -> A.Key -> b
    getForKey a k = case A.parse (`parseForKey` k) a of
        Error s -> error s
        Success b -> b
parseAesonic :: String -> (A.Object -> a) -> Value -> A.Parser a
parseAesonic n fun = A.withObject n (\o -> do
    t <- o A..: "@type"
    if t == n
        then return (fun o)
        else fail ("the '@type' does not match " ++ n))

class FromJSON a => ElementC a where
    spdxid :: a -> IRI
    default spdxid :: (Aesonic a) => a -> IRI
    spdxid = (`getForKey` "SPDXID")
    name :: a -> Maybe Text
    default name :: (Aesonic a) => a -> Maybe Text
    name = (`getForKey` "name")
    summary :: a -> Maybe Text
    default summary :: (Aesonic a) => a -> Maybe Text
    summary = (`getForKey` "summary")
newtype Element = ElementT A.Object
instance Aesonic Element where
    toObject (ElementT o) = o
instance FromJSON Element where
    parseJSON = parseAesonic "Element" ElementT
instance ElementC Element

type Actor = Element
class ArtifactC a where
    originatedBy :: a -> Actor
    default originatedBy :: (Aesonic a) => a -> Actor
    originatedBy = (`getForKey` "originatedBy")
newtype Artifact = ArtifactT A.Object
instance FromJSON Artifact where
    parseJSON = parseAesonic "Artifact" ArtifactT
instance Aesonic Artifact where
    toObject (ArtifactT o) = o
instance ElementC Artifact

newtype Package = PackageT A.Object
instance FromJSON Package where
    parseJSON = parseAesonic "Package" PackageT
instance Aesonic Package where
    toObject (PackageT o) = o
instance ElementC Package

data SPDX3
    = Element !Element
    | Artifact !Artifact
    | Package !Package
instance Aesonic SPDX3 where
    toObject (Element o) = toObject o
    toObject (Artifact o) = toObject o
    toObject (Package o) = toObject o
instance FromJSON SPDX3 where
    parseJSON v = withObject "SPDX3" (\o -> do
        t <- (o A..: "@type") :: A.Parser String
        case t of
            "Element" -> Element <$> parseJSON v
            "Artifact" -> Artifact <$> parseJSON v
            "Package" -> Package <$> parseJSON v
            _ -> fail "unsupported '@type'"
        ) v


genSPDX3Example :: SPDX3
genSPDX3Example = undefined