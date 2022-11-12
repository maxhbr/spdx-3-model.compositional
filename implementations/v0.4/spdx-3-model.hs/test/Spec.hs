{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import SPDX3.Model
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    example <- mkExample
    putStrLn ""
    print example
    putStrLn ""
    let encoded = encodePretty example
    B.putStrLn encoded
    putStrLn ""
    let parsed = eitherDecode encoded :: Either String (SPDX ())
    print parsed
    putStrLn ""
    defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests, testsOnExample]

unitTests = testGroup "Unit tests"
  [ 
  ]
testsOnExample = let
    exmaple = mkExample
    in testGroup "tests on example"
    []