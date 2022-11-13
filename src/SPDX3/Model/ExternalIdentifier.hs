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

module SPDX3.Model.ExternalIdentifier
    where
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict  as Map
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           GHC.Word             (Word8)
import           SPDX3.Model.Common

type ExternalIdentifierType = String -- TODO
data ExternalIdentifier where
  ExternalIdentifier :: {_externalIdentifierType :: ExternalIdentifierType
                        ,_externalIdentifierIdentifier :: IRI
                        ,_externalIdentifierComments :: Maybe String
                        } -> ExternalIdentifier
  deriving (Generic, Show)
instance ToJSON ExternalIdentifier where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ExternalIdentifier
