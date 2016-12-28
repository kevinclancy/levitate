{-# LANGUAGE ForeignFunctionInterface #-}
module Levitate where

{-@ assume (+↓) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Float | v = a +↓ b} @-}
foreign import ccall "add_down" (+↓) :: Float -> Float -> Float

{-@ assume (+↑) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Float | v = a +↑ b} @-}
foreign import ccall "add_up"   (+↑) :: Float -> Float -> Float

{-@ assume (-↓) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Float | v = a -↓ b} @-}
foreign import ccall "sub_down" (-↓) :: Float -> Float -> Float

{-@ assume (-↑) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Float | v = a -↑ b} @-}
foreign import ccall "sub_up"   (-↑) :: Float -> Float -> Float

{-@ assume (*↓) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Float | v = a *↓ b} @-}
foreign import ccall "mul_down" (*↓) :: Float -> Float -> Float

{-@ assume (*↑) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Float | v = a *↑ b} @-}
foreign import ccall "mul_up"   (*↑) :: Float -> Float -> Float

{-@ assume (/↓) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Float | v = a /↓ b} @-}
foreign import ccall "div_down" (/↓) :: Float -> Float -> Float

{-@ assume (/↑) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Float | v = a /↑ b} @-}
foreign import ccall "div_up"   (/↑) :: Float -> Float -> Float

-- (.=)

-- use this for all runtime floating-point comparisons instead of (==)
-- do not pattern match on float literals (which is equivalent to using ==)

{-@ assume (.=) :: a:{v:Float | true} -> b:{v:Float | true} -> {v:Bool | (Prop v) <=> a .= b}  @-}
foreign import ccall "ieee_equality" (.=) :: Float -> Float -> Bool


{-@ pinf :: {v:Float | v = (pinf 0)}  @-}
pinf :: Float
pinf = (1.0)/(0.0)

{-@ ninf :: {v:Float | v = (ninf 0)} @-}
ninf :: Float
ninf = -pinf

{-@ pzero :: {v:Float | v = (pzero 0)} @-}
pzero :: Float
pzero = 0.0

{-@ nzero :: {v:Float | v = (nzero 0)} @-}
nzero :: Float
nzero = 1.0/ninf

{-@ nonNan :: x:Float -> {v:Bool | (Prop v) <=> not (isNan x)} @-}
nonNan :: Float -> Bool
nonNan x = (x .= x) 

