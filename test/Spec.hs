{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.FileEmbed             (embedFile)
import           System.FilePath
import           System.Directory
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      as QC
import           Test.Tasty.SmallCheck      as SC

import           Data.List
import           Data.Ord

import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Either                (isRight)
import           SPDX3.From2
import           SPDX3.Model
import           SPDX3.Monad                (mkExample)

testOutputFolder :: FilePath
testOutputFolder = "_testOut"

spdxFileBS :: BSL.ByteString
spdxFileBS = BSL.fromStrict $(embedFile "spdx-tools-hs/spdx-spec/examples/SPDXJSONExample-v2.3.spdx.json")

spdxYamlFileBS :: BSL.ByteString
spdxYamlFileBS = BSL.fromStrict $(embedFile "spdx-tools-hs/spdx-spec/examples/SPDXYAMLExample-2.3.spdx.yaml")

otherSpdxYamlFileBS :: BSL.ByteString
otherSpdxYamlFileBS = BSL.fromStrict $(embedFile "spdx-tools-hs/test/data/document.spdx.yml")

main :: IO ()
main = do
  createDirectoryIfMissing True testOutputFolder
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, testsOnExample, testsFrom2]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
  testGroup "ToJSON and FromJSON" [
      testCase "testExample" $ do
        example <- mkExample
        -- print example
        let encoded = encodePretty example
        B.writeFile (testOutputFolder </> "example" <.> ".spdx3.json") encoded
        let parsed = eitherDecode encoded :: Either String (SPDX ())
        assertBool "should succeed parsing" (isRight parsed)
        -- print parsed
    ]
  ]
testsOnExample :: TestTree
testsOnExample = testGroup "tests on example" []
testsFrom2 :: TestTree
testsFrom2 = let
     bss = [ ("SPDXJSONExample-v2.2.spdx.json",spdxFileBS)
           , ("SPDXYAMLExample-2.2.spdx.yaml", spdxYamlFileBS)
           , ("document.spdx.yml", otherSpdxYamlFileBS)
           ]
  in testGroup "test conversation from 2" $ map (\(fn, bs) -> testCase ("parse BS for " ++ fn) $ do
        let result = convertBsDocument bs
        assertBool ("should succeed conversion " ++ fn) (isRight result)
        case result of
          Right success -> do
            let encoded = encodePretty success
            B.writeFile (testOutputFolder </> fn <.> ".spdx3.json") encoded
            let parsed = eitherDecode encoded :: Either String (SPDX ())
            assertBool ("should succeed parsing " ++ fn) (isRight parsed)
          Left _ -> pure ()
      ) bss
