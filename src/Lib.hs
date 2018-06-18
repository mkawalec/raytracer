{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnboxedTuples #-}

module  Lib (someFunc) where

import Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as M
import Data.Massiv.Array.IO
import Graphics.ColorSpace
import Data.Tuple.HT (uncurry3)
import qualified Debug.Trace as DT
import qualified Data.List as L
import Control.DeepSeq (force, deepseq)
import GHC.Generics (Generic)
import System.Random

type MyPixel = (Double, Double, Double)

data Ray = Ray {
  origin :: Array D Ix1 Double,
  direction :: Array D Ix1 Double
} deriving (Eq, Show)

pointAtParameter :: Ray -> Double -> Array D Ix1 Double
pointAtParameter (Ray a b) t = a + (singleton Seq t) * b

unitVector :: Array D Ix1 Double -> Array D Ix1 Double
unitVector v = v / (singleton Seq 3.0)

fromList' :: forall a. Prim a => [a] -> Array D Ix1 a
fromList' l = delay vec
  where vec = fromList Seq l :: Array P Ix1 a

fromListN' :: forall a. [a] -> Array D Ix1 a
fromListN' l = delay vec
  where vec = fromList Seq l :: Array B Ix1 a

dot :: Array D Ix1 Double -> Array D Ix1 Double -> Double
dot a b = A.sum $ A.zipWith (*) a b

color :: Hitable a => Ray -> a -> Array P Ix1 Double
color r@(Ray o d) world = 
  let unitDirection = computeAs P $ unitVector d
      t = 0.5 * (unitDirection ! 1) + 1.0
      skyColor = compute $ (singleton Seq $ 1.0 - t) * (fromList' [1.0, 1.0, 1.0]) 
                  +
               (singleton Seq t) * (fromList' [0.5, 0.7, 1.0])

      sphereColor normal = let n = computeAs P $ unitVector normal
                           in (singleton Seq 0.5) * (fromList' [
                                (n ! 0) + 1, (n ! 1) + 1, (n ! 2) + 1])
  in case hit 0.0 (1/0) world r of
     Nothing -> skyColor
     Just (HitResult _ _ normal) -> compute $ sphereColor normal
{-# INLINE color #-}

data HitResult = HitResult {
  t :: !Double,
  p :: !(Array D Ix1 Double),
  normal :: !(Array D Ix1 Double)
} deriving (Eq, Show, Ord)

data Sphere = Sphere {
  center :: !(Array D Ix1 Double),
  radius :: !Double
} deriving (Eq, Show, Ord, Generic)

newtype World a = World (Array D Ix1 a)

class Hitable a where
  hit :: Double -> Double -> a -> Ray -> Maybe HitResult

instance Hitable a => Hitable (World a) where
  hit tMin tMax (World elems) ray = A.foldlS combine Nothing elems
    where combine Nothing elem                  = hit tMin tMax elem ray
          combine (Just r@(HitResult t _ _)) elem = case hit tMin t elem ray of
            Nothing -> Just r
            Just res -> Just res

instance Hitable Sphere where
  hit tMin tMax (Sphere center r) ray@(Ray origin direction) = 
    if disc < 0
    then Nothing
    else L.find (\root -> root < tMax && root > tMin) [negativeRoot, positiveRoot] >>=
         (\root -> let p = pointAtParameter ray root 
                   in Just $ HitResult root p ((p - center) / (singleton Seq r)))
    where oc = origin - center
          a  = dot direction direction
          b = 2.0 * dot oc direction
          c = (dot oc oc) - r * r
          disc = b * b - 4 * a * c
          positiveRoot = (-b + (sqrt $ b*b - a*c)) / a
          negativeRoot = (-b + (sqrt $ b*b - a*c)) / a

arrLightIx2 :: Hitable a => Ix2 -> World a -> Int -> Array U Ix2 MyPixel
arrLightIx2 arrSz@(sizeY :. sizeX) world samples = makeArray Par arrSz lightFunc
  where origin = fromList' [0.0, 0.0, 0.0]
        vertical = fromList' [0.0, 2.0, 0.0]
        horizontal = fromList' [4.0, 0.0, 0.0]
        lowerLeft = fromList' [-2.0, -1.0, -1.0]
        samplesD = fromIntegral samples

        castRay :: Int -> Int -> (Double, Double, Double) -> Int -> (Double, Double, Double)
        castRay i j (r, g, b) sample = 
          let idx = 2 * i * j * sample
              generator = mkStdGen idx
              r1:r2:_ = randoms generator
              u = (r1 + fromIntegral i) / fromIntegral sizeX
              v = (r2 + fromIntegral (sizeY - 1 - j)) / fromIntegral sizeY
              ray = Ray origin (lowerLeft + (singleton Seq u) * horizontal + (singleton Seq v) * vertical)
              col = color ray world
          in (r + (col ! 0), g + (col ! 1), b + (col ! 2))
        lightFunc (j :. i) = let (r, g, b) = L.foldl' (castRay i j) (0.0, 0.0, 0.0) 
                                                      [0..(samples - 1)]
                             in (r / samplesD, g / samplesD, b / samplesD)
  -- sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE arrLightIx2 #-}

-- Use NFData for all these tight loops

-- PixelRGB 1 0 0
someFunc :: IO ()
someFunc = do
  stdGen <- getStdGen

  let world = World $ fromListN' [(Sphere (fromListN' [0, 0, -1]) 0.5), 
                                  (Sphere (fromListN' [0, -100.5, -1]) 100)]
      arr = arrLightIx2 (800 :. 1600) world 100
      img = computeAs S $ fmap (uncurry3 PixelRGB) $ (delay arr)
  writeImage "files/light.png" (img :: Array S Ix2 (Pixel RGB Double))
