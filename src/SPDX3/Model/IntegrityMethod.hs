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

module SPDX3.Model.IntegrityMethod
    where
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict  as Map
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           GHC.Word             (Word8)

type HashAlgorithm  = String
data IntegrityMethod where
    Hash :: Maybe String -> HashAlgorithm -> [Word8] -> IntegrityMethod
  deriving (Generic, Show)
instance ToJSON IntegrityMethod where
    toJSON (Hash comment algorithm hashValue) = object ["comment" .= comment
                                                       ,"algorithm" .= algorithm
                                                       ,"hashValue" .= hashValue
                                                       ]
instance FromJSON IntegrityMethod where
    parseJSON = withObject "IntegrityMethod" $ \o -> do
        Hash <$> o .:? "comment"
             <*> o .: "algorithm"
             <*> o .: "hashValue"
