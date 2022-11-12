{-# LANGUAGE DeriveGeneric #-}

module SPDX3.SPDXID
    where
import GHC.Generics (Generic)
import Data.Aeson
import SPDX3.RelationshipType

type SPDXID = String
class HasSPDXID a where
    getSPDXID :: a -> SPDXID
    -- toRef :: a -> Element
    -- toRef = ElementRef . getSPDXID
