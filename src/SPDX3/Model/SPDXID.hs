module SPDX3.Model.SPDXID where

type SPDXID = String
class HasSPDXID a where
    getSPDXID :: a -> SPDXID
