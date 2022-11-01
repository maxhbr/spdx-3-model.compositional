{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import SPDX3.Model
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    let example = mkExample
    putStrLn ""
    print example
    putStrLn ""
    B.putStrLn $ encodePretty example
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