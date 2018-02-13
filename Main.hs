module Main where

import Data.List (intercalate)

import Types
import Operators (defaultOperators)
import Defaults (defaultConfiguration)
import Runtime (randomRun, runN)

prettify :: Population -> String
prettify p = intercalate "|" (map show p)

main :: IO ()
main = do
  let ?config = defaultConfiguration
  let ?op = defaultOperators
  putStr $ unlines $ prettify <$> randomRun runN
