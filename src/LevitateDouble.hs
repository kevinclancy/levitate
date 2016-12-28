{-# LANGUAGE ForeignFunctionInterface #-}
module LevitateDouble where

foreign import ccall "add_down_d" (+↓) :: Double -> Double -> Double
foreign import ccall "add_up_d"   (+↑) :: Double -> Double -> Double
foreign import ccall "sub_down_d" (-↓) :: Double -> Double -> Double
foreign import ccall "sub_up_d"   (-↑) :: Double -> Double -> Double
foreign import ccall "mul_down_d" (*↓) :: Double -> Double -> Double
foreign import ccall "mul_up_d"   (*↑) :: Double -> Double -> Double
foreign import ccall "div_down_d" (/↓) :: Double -> Double -> Double
foreign import ccall "div_up_d"   (/↑) :: Double -> Double -> Double
foreign import ccall "sqrt_up_d"   sqrt_up   :: Double -> Double
foreign import ccall "sqrt_down_d" sqrt_down :: Double -> Double
foreign import ccall "exp_up_d"    exp_up   :: Double -> Double
foreign import ccall "exp_down_d"  exp_down :: Double -> Double
foreign import ccall "log_up_d"    log_up   :: Double -> Double
foreign import ccall "log_down_d"  log_down :: Double -> Double
foreign import ccall "pow_up_d"    pow_up   :: Double -> Double -> Double
foreign import ccall "pow_down_d"  pow_down :: Double -> Double -> Double
foreign import ccall "ieee_equality_d" (.=) :: Double -> Double -> Bool
foreign import ccall "getMaxFloat_d" getMaxFloat :: Int -> Double

pinf :: Double
pinf = (1.0)/(0.0)

ninf :: Double
ninf = -pinf

pzero :: Double
pzero = 0.0

nzero :: Double
nzero = 1.0/ninf

nonNan :: Double -> Bool
nonNan x = (x .= x) 

maxFloat :: Double
maxFloat = getMaxFloat 0
