module Main where

import Control.Arrow (first)
import Control.Monad.Identity

import Data.Array.Repa hiding (map)
import Data.Array.Repa.IO.BMP

import Data.Complex
import Data.Word (Word8)

import Linear

import System.Environment

import Degree
import Func

main :: IO ()
main =
  do
    [filePath,resS,aaS] <- getArgs
    let res = read resS :: Int
        aa = read aaS :: Int
        resV = V2 res res
        dim = Z :. res :. res
        aaSq = aa'*aa' where aa' =  fromIntegral aa

        aaoffsets =
          map (fmap ((/aaSq) . fromIntegral))
          [V2 x y | x <- [0..aa-1], y <- [0..aa-1]]
        image = runIdentity $ computeUnboxedP $
          fromFunction dim
          (\(Z :. y :. x) ->
             let
               V3 r g b =
                 fmap (round . (*255) . (/aaSq)) $ sum $
                 map (renderPoint . positionPoint resV (V2 y x)) aaoffsets
             in
               (r,g,b)
          )
    writeImageToBMP filePath image

bLimit :: Double
bLimit = 256

bsq :: Double
bsq = bLimit*bLimit

iLimit :: Int
iLimit = 200

positionPoint
  :: V2 Int
  -> V2 Int
  -> V2 Double
  -> V2 Double
positionPoint res loc aaoffset = (-res' + 2 * (loc' + aaoffset)) / res'
  where res' = fmap fromIntegral res
        loc' = fmap fromIntegral loc

mandelbrot :: Complex Double -> (Int, Complex Double) -> (Int, Complex Double)
mandelbrot c  = go
  where
    go v@(iter,z@(r :+ i))
      | iter == iLimit || r*r + i*i > bsq = v
      | otherwise = go (iter+1,z')
      where z' = func c z

renderPoint :: V2 Double -> V3 Double
renderPoint (V2 y x) =
  let
    (i,z) = first fromIntegral $ mandelbrot (x :+ y) (0,0)
    sl = i - (log (log (magnitude z)/log bLimit)/log order)
    transform c = 0.5 + 0.5*(cos $ 3.0 + sl*0.15 + c)
    r = transform 0
    g = transform 0.6
    b = transform 1
  in
    V3 r g b
