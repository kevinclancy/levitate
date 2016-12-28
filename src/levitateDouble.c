#include "fenv.h"
#include "math.h"
#include "float.h"

// return the rounded-down result of adding two arguments,
// where the real value of the sum of the arguments is rounded down
// to the nearest lower floating-point number.
// 
// cleans up after itself by setting the rounding mode to RoundToNearest,
// which is the default mode supported by haskell
double add_down_d(double a, double b)
{
  double ret;
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
double add_up_d(double a, double b)
{
  double ret;
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
double sub_down_d(double a, double b)
{
  double ret;
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
double sub_up_d(double a, double b)
{
  double ret;
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
double mul_down_d(double a, double b)
{
  double ret;
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
double mul_up_d(double a, double b)
{
  double ret;
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
double div_down_d(double a, double b)
{
  double ret;
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
double div_up_d(double a, double b)
{
  double ret;
  fesetround(FE_UPWARD);
  ret = a / b;
  fesetround(FE_TONEAREST);
  return ret;
}

double sqrt_down_d(double a)
{
  double ret;
  fesetround(FE_DOWNWARD);
  ret = sqrt(a);
  fesetround(FE_TONEAREST);
  return ret;
}

double sqrt_up_d(double a)
{
  double ret;
  fesetround(FE_UPWARD);
  ret = sqrt(a);
  fesetround(FE_TONEAREST);
  return ret;
}

double exp_up_d(double a)
{
  double ret;
  fesetround(FE_UPWARD);
  ret = exp(a);
  fesetround(FE_TONEAREST);
  return ret;
}

double exp_down_d(double a)
{
  double ret;
  fesetround(FE_DOWNWARD);
  ret = sqrt(a);
  fesetround(FE_TONEAREST);
  return ret;
}

double log_down_d(double a)
{
  double ret;
  fesetround(FE_DOWNWARD);
  ret = log(a);
  fesetround(FE_TONEAREST);
  return ret;
}

double log_up_d(double a)
{
  double ret;
  fesetround(FE_UPWARD);
  ret = log(a);
  fesetround(FE_TONEAREST);
  return ret;
}

double pow_down_d(double b, double e)
{
  double ret;
  fesetround(FE_DOWNWARD);
  ret = pow(b,e);
  fesetround(FE_TONEAREST);
  return ret;
} 

double pow_up_d(double b, double e)
{
  double ret;
  fesetround(FE_UPWARD);
  ret = pow(b,e);
  fesetround(FE_TONEAREST);
  return ret;
} 

// returns true whenever the two arguments are equal
// this exists to workaround issues with liquid haskell:
// we need both ieee-equality (for haskell's builtin == operator) 
// and bitwise equality (so that we can distinguish between +0 and -0)
// so the hack workaround is to make bitwise equality the default in liquidhaskell refinements,
// and implement a special function to perform ieee-equality.  
int ieee_equality_d(double a, double b)
{
  return a == b;
}

double getMaxFloat_d(int x)
{
  return DBL_MAX;
}
