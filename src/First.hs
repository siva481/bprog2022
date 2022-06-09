module First where

double :: Int ->Int
double x = x + x

quadrouple :: Int -> Int
quadrouple x = double (double x)
