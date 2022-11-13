{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module SPDX3.Model.ExternalReference
    where
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict  as Map
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           GHC.Word             (Word8)
import           SPDX3.Model.Common

type ExternalReferenceType = String -- TODO
data ExternalReference where
  ExternalReference :: {_externalReferenceType :: ExternalReferenceType
                       ,_externalReferenceLocator :: IRI
                       ,_externalReferenceContentType :: Maybe MediaType
                       ,_externalReferenceComments :: Maybe String
                       } -> ExternalReference
  deriving (Generic, Show)
instance ToJSON ExternalReference where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ExternalReference
