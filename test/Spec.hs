{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import SPDX3.Model
import SPDX3.Example (mkExample)

main :: IO ()
main = do
    putStrLn ""
    print mkExample
    putStrLn ""
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, testsOnExample]

unitTests = testGroup "Unit tests"
  [ testCase "test getSPDXID for element" $ let
      spdxid = "urn://some/spdx/id"
      element = mkElement spdxid
    in getSPDXID element @?= spdxid
  , testCase "test getSPDXID for package" $ let
      spdxid = "urn://some/spdx/id"
      actor = mkElement "urn://actor"
      package = mkPackage (mkArtifact (mkElement spdxid) [actor]) "Purpose"
    in getSPDXID package @?= spdxid
  ]
testsOnExample = let
    exmaple = mkExample
    in testGroup "tests on example"
    []