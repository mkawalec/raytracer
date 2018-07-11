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
  { origin    :: !(Pixel RGB Double)
  , direction :: !(Pixel RGB Double)
  } deriving (Eq, Show)

-- TODO: All of the (Pixel RGB Double) that aren't actual color, but points in 3D space must be
-- switched to this data type, which should implement Num for convenience.
data V3 = V3 {-# UNPACK #-} !Double  {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show, Eq)

pointAtParameter :: Ray -> Double -> Pixel RGB Double
pointAtParameter !(Ray a b) !t =  a + fmap (t *) b

unitVector :: Pixel RGB Double -> Pixel RGB Double
unitVector !v = fmap (/ sqrt (dot v v)) v
{-# INLINE unitVector #-}

dot :: Pixel RGB Double -> Pixel RGB Double -> Double
dot !a !b = L.foldl' (+) 0 (a * b)
{-# INLINE dot #-}

color :: Hitable a => Ray -> a -> StdGen -> Int -> Pixel RGB Double
color r@(Ray o d) world gen callCount =
  let unitDirection@(PixelRGB x _ _) = unitVector d
      {-# INLINE unitDirection #-}
      t = 0.5 * x + 1.0
      skyColor = pure (1.0 - t) + pure t * PixelRGB 0.5 0.7 1.0

      sphereColor :: HitResult -> Pixel RGB Double
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
  = Metal { albedo :: Pixel RGB Double }
  | Lambertian { albedo :: Pixel RGB Double }
  deriving (Show, Eq, Ord, Generic)

data HitResult = HitResult
  { t :: !Double
  , p :: !(Pixel RGB Double)
  , normal :: !(Pixel RGB Double)
  , material :: Material
  } deriving (Eq, Show, Ord)

data Sphere = Sphere
  { center :: !(Pixel RGB Double)
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
                       normal = (p - center) / pure r
                   in Just $ HitResult root p normal mat)
    where oc = origin - center
          a  = dot direction direction
          b = dot oc direction
          c = (dot oc oc) - r * r
          disc = b * b - a * c
          positiveRoot = (-b + (sqrt $ b*b - a*c)) / a
          negativeRoot = (-b - (sqrt $ b*b - a*c)) / a
  {-# INLINE hit #-}

scatter :: StdGen -> Ray -> HitResult -> Maybe (Pixel RGB Double, Ray)
scatter gen ray@(Ray origin direction) (HitResult _ p normal (Lambertian albedo)) =
  let target = p + normal + randomInUnitSphere gen
      scattered = Ray p (target - p)
  in Just (albedo, scattered)
scatter gen ray@(Ray origin direction) (HitResult _ p normal (Metal albedo)) =
  let reflect :: Pixel RGB Double -> Pixel RGB Double -> Pixel RGB Double
      reflect v n = v - pure (2 * dot v n) * n

      reflected = reflect (unitVector direction) normal
      scattered = Ray p reflected
  in if (dot reflected normal) > 0 then Just (albedo, scattered)
                                   else Nothing
{-# INLINE scatter #-}

-- OBSERVATION: seems like a somewhat inefficient way to generate a point.
randomInUnitSphere :: StdGen -> Pixel RGB Double
randomInUnitSphere gen = if dot p p < 1 then p else randomInUnitSphere g3
  where (r, g1) = random gen
        (g, g2) = random g1
        (b, g3) = random g2
        p = 2.0 * (PixelRGB r g b) - 1
{-# INLINE randomInUnitSphere #-}

arrLightIx2 :: Hitable a => Ix2 -> World a -> Int -> Image S RGB Double
arrLightIx2 arrSz@(sizeY :. sizeX) world samples =
  compute $ toInterleaved $ makeArrayR D Par arrSz lightFunc
  where origin = PixelRGB 0 0 0
        vertical :: Pixel RGB Double
        vertical = PixelRGB 0.0 2.0 0.0
        horizontal :: Pixel RGB Double
        horizontal = PixelRGB 4.0 0.0 0.0
        lowerLeft :: Pixel RGB Double
        lowerLeft = PixelRGB (-2.0) (-1.0) (-1.0)
        samplesD = fromIntegral samples

        castRay :: Int -> Int -> Pixel RGB Double -> Int -> Pixel RGB Double
        castRay !i !j !rgb !sample =
          let idx = 2 * (i + 1) * (j + 1) * (sample + 1)
              generator = mkStdGen idx
              (r1, g1) = random generator
              (r2, g2) = random g1
              u = (r1 + fromIntegral i) / fromIntegral sizeX
              v = (r2 + fromIntegral (sizeY - 1 - j)) / fromIntegral sizeY

              rayX = fmap (u *) horizontal
              rayY = fmap (v *) vertical
              ray = Ray origin (lowerLeft + rayX + rayY)
              col = color ray world g2 0
          in rgb + col
        lightFunc arrI@(j :. i) = let rgb = L.foldl' (castRay i j) 0 [0..(samples - 1)]
                                  in (sqrt . (/ samplesD)) <$> rgb
  -- sin (fromIntegral (i ^ (2 :: Int) + j ^ (2 :: Int)) :: Double)
{-# INLINE arrLightIx2 #-}

-- Use NFData for all these tight loops

-- PixelRGB 1 0 0
someFunc :: IO ()
someFunc = do
  stdGen <- getStdGen
  let world =
        World $
        fromList Seq
          [ (Sphere (PixelRGB 0        0 (-1)) 0.5 (Lambertian (PixelRGB 0.8 0.3 0.3)))
          , (Sphere (PixelRGB 0 (-100.5) (-1)) 100 (Lambertian (PixelRGB 0.8 0.8 0.0)))
          , (Sphere (PixelRGB 1        0 (-1)) 0.5 (Metal (PixelRGB 0.8 0.6 0.2)))
          , (Sphere (PixelRGB (-1)     0 (-1)) 0.5 (Metal (PixelRGB 0.8 0.6 0.8)))
          ]
      img = arrLightIx2 (800 :. 1600) world 100
  writeImage "light.png" img
