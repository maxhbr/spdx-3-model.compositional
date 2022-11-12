{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      as QC
import           Test.Tasty.SmallCheck      as SC

import           Data.List
import           Data.Ord

import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import           SPDX3.Model

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

unitTests = testGroup "Unit tests" []
testsOnExample = let exmaple = mkExample in testGroup "tests on example" []
