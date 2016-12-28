#include "fenv.h"

// return the rounded-down result of adding two arguments,
// where the real value of the sum of the arguments is rounded down
// to the nearest lower floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
float add_down(float a, float b)
{
  float ret;
  fesetround(FE_DOWNWARD);
  ret = a + b;
  fesetround(FE_TONEAREST);
  return ret;
}


// return the rounded-up result of adding two arguments,
// where the real value of the sum of the arguments is rounded up
// to the nearest higher floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
float add_up(float a, float b)
{
  float ret;
  fesetround(FE_UPWARD);
  ret = a + b;
  fesetround(FE_TONEAREST);
  return ret;
}


// return the rounded-down result of subtracting two arguments,
// where the real value of the difference of the arguments is rounded down
// to the nearest lower floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
float sub_down(float a, float b)
{
  float ret;
  fesetround(FE_DOWNWARD);
  ret = a - b;
  fesetround(FE_TONEAREST);
  return ret;
}


// return the rounded-up result of subtracting two arguments,
// where the real value of the difference of the arguments is rounded up
// to the nearest higher floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
float sub_up(float a, float b)
{
  float ret;
  fesetround(FE_UPWARD);
  ret = a - b;
  fesetround(FE_TONEAREST);
  return ret;
}

// return the rounded-down result of multiplying two arguments,
// where the real value of the product of the arguments is rounded down
// to the nearest lower floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
float mul_down(float a, float b)
{
  float ret;
  fesetround(FE_DOWNWARD);
  ret = a * b;
  fesetround(FE_TONEAREST);
  return ret;
}


// return the rounded-up result of adding two arguments,
// where the real value of the product of the arguments is rounded up
// to the nearest higher floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
float mul_up(float a, float b)
{
  float ret;
  fesetround(FE_UPWARD);
  ret = a * b;
  fesetround(FE_TONEAREST);
  return ret;
}


// return the rounded-down result of dividing two arguments,
// where the real value of the quotient of the arguments is rounded down
// to the nearest lower floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
float div_down(float a, float b)
{
  float ret;
  fesetround(FE_DOWNWARD);
  ret = a / b;
  fesetround(FE_TONEAREST);
  return ret;
}


// return the rounded-up result of dividing two arguments,
// where the real value of the quotient of the arguments is rounded up
// to the nearest higher floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
float div_up(float a, float b)
{
  float ret;
  fesetround(FE_UPWARD);
  ret = a / b;
  fesetround(FE_TONEAREST);
  return ret;
}


// returns true whenever the two arguments are equal
// this exists to workaround issues with liquid haskell:
// we need both ieee-equality (for haskell's builtin == operator) 
// and bitwise equality (so that we can distinguish between +0 and -0)
// so the hack workaround is to make bitwise equality the default in liquidhaskell refinements,
// and implement a special function to perform ieee-equality.  
int ieee_equality(float a, float b)
{
  return a == b;
}
