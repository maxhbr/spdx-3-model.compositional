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
import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Text.Hex

readHashHexValue :: String -> Maybe ByteString
readHashHexValue = decodeHex . T.pack

writeHashValueAsHex :: ByteString -> String
writeHashValueAsHex = T.unpack . encodeHex

type HashAlgorithm  = String
data IntegrityMethod where
    Hash :: Maybe String -> HashAlgorithm -> ByteString -> IntegrityMethod
  deriving (Generic, Show)
instance ToJSON IntegrityMethod where
    toJSON (Hash comment algorithm hashValue) = object ["comment" .= comment
                                                       ,"algorithm" .= algorithm
                                                       ,"hashValue" .= writeHashValueAsHex hashValue
                                                       ]
instance FromJSON IntegrityMethod where
    parseJSON = withObject "IntegrityMethod" $ \o -> do
        hashValueHex <- o .: "hashValue"
        case readHashHexValue hashValueHex of
            Just hashValue -> Hash <$> o .:? "comment"
                                   <*> o .: "algorithm"
                                   <*> return hashValue
            Nothing -> fail ("failed to parse hex string=" ++ hashValueHex)

mkHash :: HashAlgorithm -> String -> IntegrityMethod
mkHash hashAlgorithm hashValueHex = case readHashHexValue hashValueHex of
    Just hashValue -> Hash Nothing hashAlgorithm hashValue
    Nothing -> Hash (Just ("failed to parse hex=" ++ hashValueHex)) hashAlgorithm mempty
