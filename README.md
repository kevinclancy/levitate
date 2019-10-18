# levitate
Utilities for verified floating-point computation in Haskell: Controlled rounding, interval arithmetic, and affine arithmetic.

# Prerequisites

- ghc version >= 7.10.2
- cabal version >= 1.22.6.0

# Installation

Enter the following console commands:

~~~
git clone https://github.com/kevinclancy/levitate/
cd levitate
cabal sandbox init
cabal install
cabal run tests
~~~

# Overview

This library is intended for the implementation of experimental numerical verification tools. As such, it is optimized for readability and simplicity rather than performance. Each controlled rounding operator records the current rounding mode, changes to the specified one, performs the operation, and then restores the original rounding mode. It isn't very efficient, but it allows a simple programming interface.

## Controlled rounding

The Levitate module provides double-precision floating-point operators with controlled rounding. (+↑) is used for upward-rounded addtion, (+↓) for downward-rounded addition, etc. For a smooth experience using these operators, you will want an editor with unicode support. I use [xah math input mode](http://ergoemacs.org/emacs/xmsi-math-symbols-input.html).

In addition to basic arithmetic operators, controlled rounding for the exponent, logarithm, and square root functions is provided.

## Interval and affine arithmetic.

The interval and affine arithmetic schemes described in [Self-validated Numerical Methods and Applications](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.36.8089&rep=rep1&type=pdf) have been implemented in the Interval and Affine modules, respectively.

The Interval.Interval datatype has a constructor E for empty intervals, and another constructor I for non-empty intervals. The low endpoint of an interval is accessed using the lp function and the high endpoint is accessed using the hp function. Basic arithmetic operators are suffixed with !: (+!) is used for interval addition, (/!) is used for interval division, etc.



