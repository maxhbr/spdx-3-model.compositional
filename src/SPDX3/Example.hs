{-# LANGUAGE OverloadedStrings #-}
module SPDX3.Example where

import qualified Data.Text as T

import SPDX3.Model

mkExample :: SPDX3
mkExample = let
        defaultActor = mkElement "urn:actor:deafult"

        somePackage name = mkPackage (mkArtifact (mkElement ("urn:package:" ++ name)) 
                                                 [defaultActor])
                                     "APPLICATION"

        relationship relationshipType nameFrom nameTos = mkRelationship (mkElement ("urn:relationship:" ++ relationshipType ++ nameFrom ++ show nameTos))
                                                                        relationshipType
                                                                        (Package (somePackage nameFrom))
                                                                        []

        collection = mkCollection (mkElement "urn:SBOM:example")
                                  [ Package $ somePackage "package1"
                                  , Package $ somePackage "package2"
                                  , Actor defaultActor
                                  , Relationship $ relationship "DEPENDS_ON" "rPackage1" ["rPackage1-1","rPackage1-2"]
                                  ]
        bundle = mkBundle collection "Context"
        bom = mkBOM bundle
        sbom = mkSBOM bom
    in SBOM sbom
