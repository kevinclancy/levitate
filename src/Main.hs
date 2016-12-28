module Main where

import Levitate
--import Test.QuickCheck
import Interval
import qualified Affine

prop_roundDown :: Float -> Float -> Bool
prop_roundDown a b = (a +↓ b) <= (a + b) 

prop_roundDownBad :: Float -> Float -> Bool
prop_roundDownBad a b = (a + b) <= (a +↓ b)

testFloat :: Float
testFloat = 3.0 +↓ 4.0

{- TODO: we need to add support for parsing floating point number is type aliases -}

{- testIntArith :: XInterval 0.0 1000 1 -> XInterval 0 1000 1 -> XInterval 0 2000000 1 -}
testIntArith :: Interval -> Interval -> Interval
testIntArith x y =
  x *! (y +! (I 1.0 1.0))

main :: IO()
main = do
  --quickCheck prop_roundDown
  --quickCheck prop_roundDownBad
  return ()

