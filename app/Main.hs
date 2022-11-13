module Main
  ( main
  ) where

import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B

import qualified SPDX.Document as SPDX2
import           SPDX3.Model
import           SPDX3.From2
import System.Environment (getArgs)
import SPDX.Document (SPDXFileType(SPDX))

main :: IO ()
main = getArgs >>= mapM_ (\fp -> do
    spdx2 <- SPDX2.parseSPDXDocument fp
    let spdx3 = convertDocument spdx2
    let encoded = encodePretty spdx3
    B.putStrLn encoded
  )
