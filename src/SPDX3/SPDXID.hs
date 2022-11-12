module SPDX3.SPDXID where

type SPDXID = String
class HasSPDXID a where
    getSPDXID :: a -> SPDXID
