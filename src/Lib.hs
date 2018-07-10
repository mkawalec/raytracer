{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
unitVector v = v / (singleton Seq (sqrt $ dot v v))

fromList' :: forall a. Prim a => [a] -> Array D Ix1 a
fromList' l = delay vec
  where vec = fromList Seq l :: Array P Ix1 a

fromListN' :: forall a. [a] -> Array D Ix1 a
fromListN' l = delay vec
  where vec = fromList Seq l :: Array B Ix1 a

dot :: Array P Ix1 Double -> Array P Ix1 Double -> Double
dot a b = A.sum $ A.zipWith (*) a b

color :: Hitable a => Ray -> a -> StdGen -> Int -> Array P Ix1 Double
color r@(Ray o d) world gen callCount = 
  let unitDirection = computeAs P $ unitVector d
      t = 0.5 * (unitDirection ! 1) + 1.0
      skyColor = compute $ (singleton Seq $ 1.0 - t) * (fromList' [1.0, 1.0, 1.0]) 
                  +
               (singleton Seq t) * (fromList' [0.5, 0.7, 1.0])

      sphereColor :: HitResult -> Array P Ix1 Double
      sphereColor hit = 
        let (g1, g2) = split gen
        in case scatter g1 r hit of
            Just (attenuation, scattered) -> compute $ attenuation * (delay $ color scattered world g2 (callCount + 1))
            Nothing -> fromList Seq [0, 0, 0]
  in case hit 0.001 (1/0) world r of
     Nothing -> skyColor
     Just hitResult -> if callCount < 6
                       then sphereColor hitResult
                       else fromList Seq [0, 0, 0]
{-# INLINE color #-}

data Material = 
  Metal { albedo :: Array D Ix1 Double } |
  Lambertian { albedo :: Array D Ix1 Double }
  deriving (Show, Eq, Ord, Generic)

data HitResult = HitResult {
  t :: !Double,
  p :: !(Array D Ix1 Double),
  normal :: !(Array D Ix1 Double),
  material :: Material
} deriving (Eq, Show, Ord)

data Sphere = Sphere {
  center :: !(Array D Ix1 Double),
  radius :: !Double,
  material :: Material
} deriving (Eq, Show, Ord, Generic)

newtype World a = World (Array D Ix1 a)

class Hitable a where
  hit :: Double -> Double -> a -> Ray -> Maybe HitResult

instance Hitable a => Hitable (World a) where
  hit tMin tMax (World elems) ray = A.foldlS combine Nothing elems
    where combine Nothing elem                  = hit tMin tMax elem ray
          combine (Just r@(HitResult t _ _ _)) elem = case hit tMin t elem ray of
            Nothing -> Just r
            Just res -> Just res

instance Hitable Sphere where
  hit tMin tMax (Sphere center r mat) ray@(Ray origin direction) = 
    if disc < 0
    then Nothing
    else L.find (\root -> root < tMax && root > tMin) [negativeRoot, positiveRoot] >>=
         (\root -> let p = pointAtParameter ray root 
                   in Just $ HitResult root p ((p - center) / (singleton Seq r)) mat)
    where oc = origin - center
          a  = dot direction direction
          b = dot oc direction
          c = (dot oc oc) - r * r
          disc = b * b - a * c
          positiveRoot = (-b + (sqrt $ b*b - a*c)) / a
          negativeRoot = (-b - (sqrt $ b*b - a*c)) / a

scatter :: StdGen -> Ray -> HitResult -> Maybe (Array D Ix1 Double, Ray)
scatter gen ray@(Ray origin direction) (HitResult _ p normal (Lambertian albedo)) = 
  let target = p + normal + (delay $ randomInUnitSphere gen)
      scattered = Ray p (target - p)
  in Just (albedo, scattered)

scatter gen ray@(Ray origin direction) (HitResult _ p normal (Metal albedo)) = 
  let reflect :: Array D Ix1 Double -> Array D Ix1 Double -> Array D Ix1 Double
      reflect v n = v - (singleton Seq (2 * (dot v n))) * n

      reflected = reflect (unitVector direction) normal
      scattered = Ray p reflected
  in if (dot reflected normal) > 0 then Just (albedo, scattered)
                                   else Nothing

randomInUnitSphere :: StdGen -> Array P Ix1 Double
randomInUnitSphere gen = if (A.sum $ A.zipWith (*) p p) < 1 then p else randomInUnitSphere g3
  where (r1, g1) = random gen
        (r2, g2) = random g1
        (r3, g3) = random g2
        p = computeAs P $ (singleton Seq 2.0) * (fromList' [r1, r2, r3]) - (fromList' [1, 1, 1])

arrLightIx2 :: Hitable a => Ix2 -> World a -> Int -> Array U Ix2 MyPixel
arrLightIx2 arrSz@(sizeY :. sizeX) world samples = makeArray Par arrSz lightFunc
  where origin = fromList' [0.0, 0.0, 0.0]
        vertical = fromList' [0.0, 2.0, 0.0]
        horizontal = fromList' [4.0, 0.0, 0.0]
        lowerLeft = fromList' [-2.0, -1.0, -1.0]
        samplesD = fromIntegral samples

        castRay :: Int -> Int -> (Double, Double, Double) -> Int -> (Double, Double, Double)
        castRay i j (r, g, b) sample = 
          let idx = 2 * (i + 1) * (j + 1) * (sample + 1)
              generator = mkStdGen idx
              (r1, g1) = random generator
              (r2, g2) = random g1
              u = (r1 + fromIntegral i) / fromIntegral sizeX
              v = (r2 + fromIntegral (sizeY - 1 - j)) / fromIntegral sizeY
              ray = Ray origin (lowerLeft + (singleton Seq u) * horizontal + (singleton Seq v) * vertical)
              col = color ray world g2 0
          in (r + (col ! 0), g + (col ! 1), b + (col ! 2))
        lightFunc arrI@(j :. i) = let (r, g, b) = L.foldl' (castRay i j) (0.0, 0.0, 0.0) 
                                                      [0..(samples - 1)]
                             in (sqrt $ r / samplesD, sqrt $ g / samplesD, sqrt $ b / samplesD)
  -- sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE arrLightIx2 #-}

-- Use NFData for all these tight loops

-- PixelRGB 1 0 0
someFunc :: IO ()
someFunc = do
  stdGen <- getStdGen

  let world = World $ fromListN' [
        (Sphere (fromListN' [0, 0, -1]) 0.5 (Lambertian (fromList' [0.8, 0.3, 0.3]))),
        (Sphere (fromListN' [0, -100.5, -1]) 100 (Lambertian (fromList' [0.8, 0.8, 0.0]))),
        (Sphere (fromListN' [1, 0, -1]) 0.5 (Metal (fromList' [0.8, 0.6, 0.2]))),
        (Sphere (fromListN' [-1, 0, -1]) 0.5 (Metal (fromList' [0.8, 0.6, 0.8])))
                                  ]
      arr = arrLightIx2 (10 :. 20) world 100
      img = computeAs S $ fmap (uncurry3 PixelRGB) $ (delay arr)
  writeImage "files/light.png" (img :: Array S Ix2 (Pixel RGB Double))
