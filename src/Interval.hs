module Interval where

import LevitateDouble

import Test.HUnit
import Test.QuickCheck.Test
import System.IO.Unsafe (unsafePerformIO)

data Interval = I {lp :: Double, hp :: Double}
              | E
              deriving (Eq, Show, Ord)

make :: Double -> Double -> Interval
make a b = if b < a then E else I (lo $ a) (hi $ b)

p0 :: Interval -> Bool
p0 E = False
p0 (I a b) = (a .= 0.0) && (b > 0.0)

p1 :: Interval -> Bool
p1 E = False
p1 (I a _) = a > pzero

n0 :: Interval -> Bool
n0 E = False
n0 (I a b) = (b .= 0.0) && (a < 0.0)

n1 :: Interval -> Bool
n1 E = False
n1 (I _ b) = b < nzero

m :: Interval -> Bool
m E = False
m (I a b) = a < 0.0 && b > 0.0

e :: Interval -> Bool
e E = True
e _ = False

p :: Interval -> Bool
p E = False
p (I a b) = a >= 0.0 && b > 0.0

n :: Interval -> Bool
n E = False
n (I a b) = a < 0.0 && b <= 0.0

z :: Interval -> Bool
z E = False
z (I a b) = a .= 0.0 && b .= 0.0

lo :: Double -> Double
lo x = if x .= 0.0 then pzero else x

hi :: Double -> Double
hi x = if x .= 0.0 then nzero else x

(+!) :: Interval -> Interval -> Interval
(+!) E _ = E
(+!) _ E = E
(+!) (I a b) (I c d) = 
  if ac .= pinf || bd .= ninf then E
  else I (lo $ ac) (hi $ bd) 
  where 
    ac = a +↓ c
    bd = b +↑ d

(-!) :: Interval -> Interval -> Interval
(-!) E _ = E
(-!) _ E = E
(-!) (I a b) (I c d) = 
  if ad .= pinf || bc .= ninf then E
  else I (lo $ ad) (hi $ bc)
  where
    ad = a -↓ d
    bc = b -↑ c

(*!) :: Interval -> Interval -> Interval
(*!) E _ = E
(*!) _ E = E
(*!) i0 i1 = case (i0,i1) of
               (I a b, I c d) | (p i0) && (p i1) -> I (a *↓ c) (b *↑ d)
               (I a b, I _ d) | (m i0) && (p i1) -> I (a *↓ d) (b *↑ d)
               (I a b, I c d) | (n i0) && (p i1) -> I (a *↓ d) (b *↑ c)
               (I _ b, I c d) | (p i0) && (m i1) -> I (b *↓ c) (b *↑ d)
               (I a b, I c d) | (m i0) && (m i1) -> I (min (a *↓ d) (b *↓ c)) (max (b *↑ d) (a *↑ c))
               (I a _, I c d) | (n i0) && (m i1) -> I (a *↓ d) (a *↑ c)
               (I a b, I c d) | (p i0) && (n i1) -> I (b *↓ c) (a *↑ d)
               (I a b, I c _) | (m i0) && (n i1) -> I (b *↓ c) (a *↑ c)
               (I a b, I c d) | (n i0) && (n i1) -> I (b *↓ d) (a *↑ c)
               (I _ _, I _ _) | (z i0) || (z i1) -> I pzero nzero
               _ -> error "unreachable" 


(/!) :: Interval -> Interval -> Interval
(/!) E _ = E
(/!) _ E = E
(/!) i0 i1 = case (i0,i1) of
               (I a b, I c d) | (p1 i0) && (p i1) -> I (a /↓ d) (b /↑ c)
               (I _ b, I c _) | (p0 i0) && (p i1) -> I pzero (b /↑ c)
               (I a b, I c _) | (m  i0) && (p i1) -> I (a /↓ c) (b /↑ c)
               (I a _, I c _) | (n0 i0) && (p i1) -> I (a /↓ c) nzero
               (I a b, I c d) | (n1 i0) && (p i1) -> I (a /↓ c) (b /↑ d)
               (I _ _, I _ _) | (not (z i0)) && (m i1) -> I ninf pinf
               (I a b, I c d) | (p1 i0) && (n i1) -> I (b /↓ d) (a /↑ c)
               (I _ b, I _ d) | (p0 i0) && (n i1) -> I (b /↓ d) nzero
               (I a b, I _ d) | (m  i0) && (n i1) -> I (b /↓ d) (a /↑ d)
               (I a _, I _ d) | (n0 i0) && (n i1) -> I pzero (a /↑ d)
               (I a b, I c d) | (n1 i0) && (n i1) -> I (b /↓ c) (a /↑ d)
               (I _ _, I _ _) | (z  i0) && (not (z i1)) -> I pzero nzero
               (I _ _, I _ _) | (z i1) -> E
               _ -> error "unreachable"

sqrt :: Interval -> Interval
sqrt E = E
sqrt i@(I a b) = make (sqrt_down a) (sqrt_up b)

exp :: Interval -> Interval
exp E = E
exp i@(I a b) = make (exp_down a) (exp_up b) 

log :: Interval -> Interval
log E = E
log i@(I a b) = make (log_down a) (log_up b) 

sin :: Interval -> Interval
sin E = E
sin _ = I (-1.0) (1.0)

cos :: Interval -> Interval
cos E = E
cos _ = I (-1.0) (1.0)

(^!) :: Interval -> Interval -> Interval
(^!) E _ = E
(^!) _ E = E
(^!) i1@(I a1 b1) i2@(I a2 b2) =
 (make (pow_down a1 a2) (pow_up b1 b2))

union :: Interval -> Interval -> Interval
union (I a b) (I c d) = (I (min a c) (max b d))

intersection :: Interval -> Interval -> Interval
intersection (I a b) (I c d) = (I (max a c) (min b d))

negate :: Interval -> Interval
negate E = E
negate (I a b) = make (-b) (-a) 

-- a point that is guaranteed to lie between endpoints, and also guaranteed not to overflow
midpoint :: Interval -> Double
midpoint E = 0.0
midpoint (I a b) | (a .= ninf) && (b .= pinf) = 0.0
midpoint (I a b) | a .= b = a
midpoint (I a _) | a .= ninf = -maxFloat
midpoint (I _ b) | b .= pinf = maxFloat
midpoint (I a b) = (a /↑ 2.0) + (b /↓ 2.0)

-- adding and subtracting the radius from the midpoint results
-- in an interval that *contains* the original interval
radius :: Interval -> Double
radius E = 0.0
radius (I a b) | a .= b = 0.0
radius (I a b) = 
  let mp = midpoint (I a b) in
  max (mp -↑ a) (b -↑ mp)

prop_inclusion :: Double -> Double -> Bool
prop_inclusion a b =
  let i = (I a b) in 
  let mp = midpoint i in
  let r = radius i in
  (mp-r <= a && mp+r >= b) 

radiusMidpointTest :: Test
radiusMidpointTest =
  TestLabel
    "radius midpoint test" $
    TestList [
      TestCase $ assertBool "inclusion prop failed" $ isSuccess (unsafePerformIO (quickCheckResult prop_inclusion))
    ]


allTests :: Test
allTests = TestList [
    radiusMidpointTest
  ]
