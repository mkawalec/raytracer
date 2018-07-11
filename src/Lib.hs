{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DuplicateRecordFields #-}

module  Lib (someFunc) where

import           Control.DeepSeq (force, deepseq)
import qualified Data.List as L
import           Data.Massiv.Array as A
import           Data.Massiv.Array.IO
import qualified Data.Massiv.Array.Mutable as M
import           Data.Tuple.HT (uncurry3)
import qualified Debug.Trace as DT
import           GHC.Generics (Generic)
import           Graphics.ColorSpace
import           System.Random

data Ray = Ray
  { origin    :: !V3
  , direction :: !V3
  } deriving (Eq, Show)

data V3 = V3 {-# UNPACK #-} !Double  {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show, Eq, Ord, Generic)

instance Num V3 where
  (V3 a1 a2 a3) + (V3 b1 b2 b3) = V3 (a1 + b1) (a2 + b2) (a3 + b3)
  (V3 a1 a2 a3) - (V3 b1 b2 b3) = V3 (a1 - b1) (a2 - b2) (a3 - b3)
  (V3 a1 a2 a3) * (V3 b1 b2 b3) = V3 (a1 * b1) (a2 * b2) (a3 * b3)
  negate (V3 a1 a2 a3) = V3 (-a1) (-a2) (-a3)
  abs v = v    -- for the lolz
  signum v = v -- as well
  fromInteger i = let asDouble = fromIntegral i in V3 asDouble asDouble asDouble

instance Fractional V3 where
  (V3 a1 a2 a3) / (V3 b1 b2 b3) = V3 (a1 / b1) (a2 / b2) (a3 / b3)
  fromRational r = let asD = fromRational r in (V3 asD asD asD)

asV3 :: Double -> V3
asV3 a = V3 a a a


pointAtParameter :: Ray -> Double -> V3
pointAtParameter !(Ray a b) !t =  a + (asV3 t) * b

unitVector :: V3 -> V3
unitVector !v = v / (asV3 $ sqrt (dot v v))
{-# INLINE unitVector #-}

dot :: V3 -> V3 -> Double
dot !a !b = let (V3 x y z) = (a * b) in x + y + z
{-# INLINE dot #-}

color :: Hitable a => Ray -> a -> StdGen -> Int -> V3
color r@(Ray o d) world gen callCount =
  let unitDirection@(V3 x _ _) = unitVector d
      {-# INLINE unitDirection #-}
      t = 0.5 * x + 1.0
      skyColor = asV3 (1.0 - t) + asV3 t * V3 0.5 0.7 1.0

      sphereColor :: HitResult -> V3
      sphereColor hit =
        let (g1, g2) = split gen
        in case scatter g1 r hit of
            Just (attenuation, scattered) ->
              attenuation * color scattered world g2 (callCount + 1)
            Nothing -> 0
      {-# INLINE sphereColor #-}
  in case hit 0.001 (1/0) world r of
     Nothing -> skyColor
     Just hitResult -> if callCount < 50
                       then sphereColor hitResult
                       else 0
{-# INLINE color #-}

data Material
  = Metal { albedo :: V3, fuzz :: Double }
  | Lambertian { albedo :: V3 }
  deriving (Show, Eq, Ord, Generic)

data HitResult = HitResult
  { t :: !Double
  , p :: !V3
  , normal :: !V3
  , material :: Material
  } deriving (Eq, Show, Ord)

data Sphere = Sphere
  { center :: !V3
  , radius :: !Double
  , material :: Material
  } deriving (Eq, Show, Ord, Generic)

newtype World a = World (Array B Ix1 a)

class Hitable a where
  hit :: Double -> Double -> a -> Ray -> Maybe HitResult

instance Hitable a => Hitable (World a) where
  hit tMin tMax (World elems) ray = A.foldlS combine Nothing elems
    where combine Nothing elem                      = hit tMin tMax elem ray
          combine (Just r@(HitResult t _ _ _)) elem = case hit tMin t elem ray of
            Nothing -> Just r
            Just res -> Just res
          {-# INLINE combine #-}
  {-# INLINE hit #-}

instance Hitable Sphere where
  hit tMin tMax (Sphere center r mat) ray@(Ray origin direction) =
    if disc < 0
    then Nothing
    else L.find (\root -> root < tMax && root > tMin) [negativeRoot, positiveRoot] >>=
         (\root -> let p = pointAtParameter ray root
                       normal = (p - center) / (asV3 r)
                   in Just $ HitResult root p normal mat)
    where oc = origin - center
          a  = dot direction direction
          b = dot oc direction
          c = (dot oc oc) - r * r
          disc = b * b - a * c
          positiveRoot = (-b + (sqrt $ b*b - a*c)) / a
          negativeRoot = (-b - (sqrt $ b*b - a*c)) / a
  {-# INLINE hit #-}

scatter :: StdGen -> Ray -> HitResult -> Maybe (V3, Ray)
scatter gen ray@(Ray origin direction) (HitResult _ p normal (Lambertian albedo)) =
  let target = p + normal + randomInUnitSphere gen
      scattered = Ray p (target - p)
  in Just (albedo, scattered)
scatter gen ray@(Ray origin direction) (HitResult _ p normal (Metal albedo fuzz)) =
  let reflect :: V3 -> V3 -> V3
      reflect v n = v - asV3 (2 * dot v n) * n

      reflected = reflect (unitVector direction) normal
      scattered = Ray p (reflected + (asV3 fuzz) * randomInUnitSphere gen)
  in if (dot reflected normal) > 0 then Just (albedo, scattered)
                                   else Nothing
{-# INLINE scatter #-}

randomInUnitSphere :: StdGen -> V3
randomInUnitSphere gen = unitVector p
  where (v1, g1) = random gen
        (v2, g2) = random g1
        (v3, _) = random g2
        p = asV3 2.0 * (V3 v1 v2 v3) - asV3 1
{-# INLINE randomInUnitSphere #-}

arrLightIx2 :: Hitable a => Ix2 -> World a -> Int -> Image S RGB Double
arrLightIx2 arrSz@(sizeY :. sizeX) world samples =
  compute $ toInterleaved $ makeArrayR D Par arrSz lightFunc
  where origin = V3 0 0 0
        vertical = V3 0 2 0
        horizontal = V3 4 0 0
        lowerLeft = V3 (-2) (-1) (-1)
        samplesD = fromIntegral samples

        castRay :: Int -> Int -> V3 -> Int -> V3
        castRay !i !j !rgb !sample =
          let idx = 2 * (i + 1) * (j + 1) * (sample + 1)
              generator = mkStdGen idx
              (r1, g1) = random generator
              (r2, g2) = random g1
              u = (r1 + fromIntegral i) / fromIntegral sizeX
              v = (r2 + fromIntegral (sizeY - 1 - j)) / fromIntegral sizeY

              rayX = asV3 u * horizontal
              rayY = asV3 v * vertical
              ray = Ray origin (lowerLeft + rayX + rayY)
              col = color ray world g2 0
          in rgb + col
        lightFunc arrI@(j :. i) = 
          let (V3 r g b) = L.foldl' (castRay i j) 0 [0..(samples - 1)]
          in PixelRGB (sqrt $ r / samplesD) (sqrt $ g / samplesD) (sqrt $ b / samplesD)
{-# INLINE arrLightIx2 #-}

someFunc :: IO ()
someFunc = do
  stdGen <- getStdGen
  let world =
        World $
        fromList Seq
          [ (Sphere (V3 0        0 (-1)) 0.5 (Lambertian (V3 0.8 0.3 0.3)))
          , (Sphere (V3 0 (-100.5) (-1)) 100 (Lambertian (V3 0.8 0.8 0.0)))
          , (Sphere (V3 1        0 (-1)) 0.5 (Metal (V3 0.8 0.6 0.2) 0.3))
          , (Sphere (V3 (-1)     0 (-1)) 0.5 (Metal (V3 0.8 0.6 0.8) 1.0))
          ]
      img = arrLightIx2 (800 :. 1600) world 100
  writeImage "light.png" img
