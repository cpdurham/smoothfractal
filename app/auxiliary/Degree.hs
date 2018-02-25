module Degree where

--gets degree of polynomial through overloading
data Degree a = Degree {getDegree :: Int, _a :: a}
              deriving Show

instance (Eq a, Num a) => Num (Degree a) where
  (*) (Degree n1 s1) (Degree n2 s2)
    | s1 == 0 || s2 == 0 = Degree 0 0
    | otherwise = Degree (n1+n2) s3
    where s3 = s1*s2
  (+) (Degree n1 s1) (Degree n2 s2)
    | n1 == n2 = Degree n1 s3
    | n1 < n2 = Degree n2 s3
    | otherwise = Degree n1 s3
    where s3 = s1+s2
  (-) (Degree n1 s1) (Degree n2 s2)
    | n1 == n2 = if s3 == 0 then Degree 0 0 else Degree n1 s3
    | n1 < n2 = Degree n2 s3
    | otherwise = Degree n1 s3
    where s3 = s1-s2
  signum (Degree n s) = Degree n (signum s)
  fromInteger i = Degree 0 (fromInteger i)
  negate (Degree n s) = Degree n (negate s)
  abs (Degree n s) = Degree n (abs s)
