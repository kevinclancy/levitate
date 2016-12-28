module Main where

import Test.HUnit
import qualified Affine
import qualified Interval

main :: IO()
main = do
  runTestTT Affine.allTests
  runTestTT Interval.allTests
  return ()

