module Affine where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Map hiding (foldl)
import Control.Monad.State
import LevitateDouble
import Prelude hiding (lookup,negate,map)
import qualified Interval 
import Interval ( (*!), (+!), (-!) )

import Test.HUnit hiding (State)

type NoiseSymbol = Int

type AffineMonad = State NoiseSymbol

data Affine = 
    Affine { center :: Double, coefficients :: Map NoiseSymbol Double }
  | RealLine
  | Empty

newNoiseSymbol :: AffineMonad NoiseSymbol
newNoiseSymbol = do
  nextSym <- get
  put (nextSym + 1)
  return nextSym

--makeFreshAffine c ks
--make an affine form with center c and noise coefficients ks for fresh noise symbols
makeFreshAffine :: Double -> [Double] -> AffineMonad Affine
makeFreshAffine c ks = do
  terms <- mapM makeFreshSym ks
  return $ (Affine c (fromList terms))
  where
    makeFreshSym :: Double -> AffineMonad (NoiseSymbol,Double)
    makeFreshSym x = do
      sym <- newNoiseSymbol
      return (sym,x)

radius :: Affine -> Double
radius Empty = 0.0
radius RealLine = pinf
radius (Affine _ ks) = 
  foldl (\acc c -> acc +↑ c) 0.0 ks   

negate :: Affine -> AffineMonad Affine
negate Empty = do return Empty
negate RealLine = do return RealLine
negate (Affine k0 ks) = do
  return (Affine (-k0) (map (\v -> -v) ks))

negateTest :: Test
negateTest =
  TestLabel
    "Negate Test" $
    TestList [
      (evalState comp1 0) ~?= 1,
      (evalState comp2 0) ~?= [(0,-1.0)]
    ]
  where
    comp1 :: AffineMonad Int
    comp1 = do
      x <- makeFreshAffine 0.0 [1.0]
      y <- negate x
      return $ size $ intersection (coefficients x) (coefficients y)

    comp2 :: AffineMonad [(Int,Double)]
    comp2 = do
      x <- makeFreshAffine 1.0 [1.0]
      y <- negate x
      return $ assocs $ coefficients y

(+@) :: Affine -> Affine -> AffineMonad Affine
(+@) Empty _ = do return Empty
(+@) _ Empty = do return Empty
(+@) RealLine _  = do return RealLine
(+@) _ RealLine  = do return RealLine
(+@) a1@(Affine center1 c1) a2@(Affine center2 c2) = do
  let (common,coefficient_roundoff) = foldl foldSymbol (empty, 0.0) i_keys
  --this is a left-biased-union, so common overwrites key collisions
  let existing_coefficients = union common (union c1 c2) 
  roundoffSym <- newNoiseSymbol
  let res_coefficients = if coefficient_roundoff +↑ res_center_roundoff /= 0.0 then 
                           insert roundoffSym (coefficient_roundoff +↑ res_center_roundoff) existing_coefficients
                         else
                           existing_coefficients
  if (res_center == pinf || res_center == ninf)
    then return RealLine
    else 
      if isJust (find (\x -> x == pinf || x == ninf) (elems res_coefficients)) 
        then return RealLine
        else return (Affine { center = res_center, coefficients = res_coefficients  })
  where
    i_keys :: [NoiseSymbol]
    i_keys = keys (intersection c1 c2)

    res_center :: Double
    res_center = center1 + center2
    
    res_center_roundoff :: Double
    res_center_roundoff = 
      let u = (center a1) +↑ (center a2) in
      let l = (center a1) +↓ (center a2) in
      max (u -↑ res_center) (res_center -↑ l) 

    -- return sum/roundoff pair of coefficients corresponding to given noise symbol
    getSumAndRoundoff :: NoiseSymbol -> (Double,Double)
    getSumAndRoundoff sym =
      let (Just x) = lookup sym c1 in
      let (Just y) = lookup sym c2 in
      let u = (x +↑ y) -↑ (x + y) in
      let l = (x + y) -↑ (x +↓ y) in
      (x + y, max u l)

    foldSymbol :: (Map NoiseSymbol Double, Double) -> NoiseSymbol -> (Map NoiseSymbol Double, Double)
    foldSymbol (m_acc,err_acc) sym =
      let (s, r) = getSumAndRoundoff sym in
      (insert sym s m_acc, r +↑ err_acc) 

plusTest :: Test
plusTest =
  TestLabel
    "plusTest" $
    TestList [
      (evalState comp1 0) ~?= [(0,2.0)]
    ]
  where
    comp1 :: AffineMonad [(NoiseSymbol,Double)]
    comp1 = do
      x <- makeFreshAffine 1.0 [1.0]
      y <- x +@ x
      return $ assocs $ coefficients y


(-@) :: Affine -> Affine -> AffineMonad Affine
(-@) a1 a2 = do
  a2' <- negate a2
  res <- a1 +@ a2'
  return res

-- scalar times affine, the dot is middot in xah mode (we'll use * for affine times affine)
(·@) :: Double -> Affine -> AffineMonad Affine
(·@) _ Empty = do return Empty
(·@) _ RealLine = do return RealLine
(·@) s (Affine c ks) = do
  let (scaled_coefficients,coefficient_roundoff) = foldl foldSymbol (empty, 0.0) (keys ks)
  let total_roundoff = coefficient_roundoff +↑ center_roundoff
  roundoffSym <- newNoiseSymbol
  let res_coefficients = if total_roundoff /= 0.0 then 
                           insert roundoffSym total_roundoff scaled_coefficients
                         else
                           scaled_coefficients
  if (scaled_center == pinf || scaled_center == ninf)
    then return RealLine
    else 
      if isJust (find (\x -> x == pinf || x == ninf) (elems res_coefficients)) 
        then return RealLine
        else return (Affine { center = scaled_center, coefficients = res_coefficients  })
  where   
    scaled_center :: Double
    scaled_center = s*c
 
    center_roundoff :: Double
    center_roundoff = 
      let u = s *↑ c in
      let l = s *↓ c in
      max (u -↑ scaled_center) (scaled_center -↑ l) 

    -- return sum/roundoff pair of coefficients corresponding to given noise symbol
    getScaledAndRoundoff :: NoiseSymbol -> (Double,Double)
    getScaledAndRoundoff sym =
      let (Just x) = lookup sym ks in
      let u = (s *↑ x) -↑ (s * x) in
      let l = (s * x) -↑ (s *↓ x) in
      (s*x, max u l)

    foldSymbol :: (Map NoiseSymbol Double, Double) -> NoiseSymbol -> (Map NoiseSymbol Double, Double)
    foldSymbol (m_acc,err_acc) sym =
      let (scaled, err) = getScaledAndRoundoff sym in
      (insert sym scaled m_acc, err +↑ err_acc) 

minusTest :: Test
minusTest =
  TestLabel
    "plusTest" $
    TestList [
      (evalState comp1 0) ~?= [(0,0.0)]
    ]
  where
    comp1 :: AffineMonad [(NoiseSymbol,Double)]
    comp1 = do
      x <- makeFreshAffine 1.0 [1.0]
      y <- x -@ x
      return $ assocs $ coefficients y

minRangeInv :: Interval.Interval -> AffineMonad (Double,Double,Double)
minRangeInv Interval.E = error "minRangeInv does not accept empty intervals"
minRangeInv i@(Interval.I lo hi) = do
  let a = max (abs lo) (abs hi)
  let b = min (abs lo) (abs hi)
  -- alpha = -↓1/b²↓ ~ why is this rounded down? to prevent overflow
  let α = - (1 /↓ (b *↑ b))
  -- dmax = ↑1/a - αa↑ ~ the y-intercept of the line of slope α going through (a,1/a)
  let dmax = (1 /↑ a) -↑ (α *↓ a)
  -- dmin = ↑1/a - αa↑ ~ the y-intercept of the line of slope α going through (a,1/a)
  let dmin = (1 /↓ b) -↓ (α *↑ a)
  let dint = Interval.I dmin dmax
  let ζ = if lo >= 0 then Interval.midpoint i else -(Interval.midpoint i)
  let δ = Interval.radius dint
  return (α,ζ,δ)
  
inv :: Affine -> AffineMonad Affine
inv Empty = do return Empty
inv RealLine = do return RealLine
inv a@(Affine _ _) = do
  let i@(Interval.I lo hi) = fromAffine a
  if lo <= 0.0 && hi >= 0.0 
    then return RealLine
    else do (α,ζ,δ) <- minRangeInv i
            r1 <- α ·@ a
            r2 <- makeFreshAffine ζ [δ]
            r3 <- r1 +@ r2
            return r3

(*@) :: Affine -> Affine -> AffineMonad Affine
(*@) Empty _ = return Empty
(*@) _ Empty = return Empty
(*@) RealLine _ = return RealLine
(*@) _ RealLine = return RealLine
(*@) x@(Affine cx _) y@(Affine cy _) = do
  let rx = radius x
  let ry = radius y
  let δ = rx *↑ ry
  let p = (Interval.make cx cx) *! (Interval.make cy cy)
  let δ' = δ +↑ (Interval.radius p)
  aζ <- makeFreshAffine (-(Interval.midpoint p)) []
  aδ <- makeFreshAffine δ' []
  a1 <- cy ·@ x
  a2 <- cx ·@ y
  a3 <- a1 +@ a2
  a4 <- a3 +@ aζ
  a5 <- a4 +@ aδ
  return a5
  
(/@) :: Affine -> Affine -> AffineMonad Affine
(/@) x y = do
  inv_y <- inv y
  res <- x *@ inv_y
  return res

fromAffine :: Affine -> Interval.Interval
fromAffine Empty = Interval.E
fromAffine RealLine = Interval.I ninf pinf 
fromAffine x@(Affine c0 _) =
   let r = radius x in 
   Interval.I (c0 -↓ r) (c0 +↑ r)

affTest1 :: Test
affTest1 = 
  TestLabel
    "affTest1" $
    TestList [
      --TODO: test 
      (evalState comp1 0) ~?= (Interval.I pzero nzero),
      (evalState comp2 0) ~?= [(0,0.0)]
    ]
  where
    comp1 :: AffineMonad Interval.Interval
    comp1 = do
      x <- makeFreshAffine 0.0 [1.0]
      y <- x -@ x
      return (fromAffine y)

    comp2 :: AffineMonad [(NoiseSymbol,Double)]
    comp2 = do
      x <- makeFreshAffine 0.0 [1.0]
      y <- x -@ x
      return $ assocs $ coefficients y


affineVsIntervalTest :: Test
affineVsIntervalTest =
  TestLabel
    "affine vs. interval" $
    TestList [
      TestCase $ assertBool "affine not narrower than interval" (evalState comp1 0)
    ]
  where
    comp1 :: AffineMonad Bool
    comp1 = do
      x <- makeFreshAffine 0.0 [2.0]
      r <- makeFreshAffine 0.0 [1.0]
      s <- makeFreshAffine 0.0 [1.0]
      
      m1   <- makeFreshAffine 10.0 []
      m1'  <- m1  +@ x
      m1'' <- m1' -@ r
     
      m2   <- makeFreshAffine 10.0 []
      m2'  <- m2  -@ x
      m2'' <- m2' +@ s

      res_aff <- m1'' *@ m2''
      let aff_rad = radius res_aff

      let ix = fromAffine x
      let ir = fromAffine r
      let is = fromAffine s
      let iten = Interval.make 10.0 10.0
      
      let res_int = (iten +! ix +! ir) *! (iten -! ix +! is)
      let int_rad = Interval.radius res_int

      return (aff_rad < int_rad)
      


allTests :: Test
allTests = TestList [
    negateTest,
    plusTest,
    minusTest,
    affTest1,
    affineVsIntervalTest
  ]
