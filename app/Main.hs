module Main (main) where

import SPDX3.Model
import SPDX3.Model (mkExample)

main :: IO ()
main = do 
    example <- mkExample
    print example
