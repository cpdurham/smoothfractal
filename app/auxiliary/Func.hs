module Func (func,order) where
import Data.Complex
import Degree

func :: Complex Double -> Complex Double -> Complex Double
func = func' c1
{-# INLINE func #-}

func' c1 c z = z^7 + (3-c)*z^3 + (c + c1)*z + c
{-# INLINE func' #-}

order :: Double
order = fromIntegral $ getDegree $ func' (Degree 1 1) (Degree 1 1) (Degree 1 1)

c1 = 1 :+ 1
{-# INLINE c1 #-}

