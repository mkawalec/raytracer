{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module  Lib (someFunc) where

import           Control.DeepSeq (force, deepseq, NFData)
import           Prelude hiding (Maybe(..))
import qualified Data.List as L
import           Data.Massiv.Array as A
import           Data.Massiv.Array.IO
import qualified Data.Massiv.Array.Mutable as M
import           Data.Tuple.HT (uncurry3)
import qualified Debug.Trace as DT
import           GHC.Generics (Generic)
import           Graphics.ColorSpace
import           Foreign.Storable
import           Foreign.Ptr (Ptr(..), plusPtr, castPtr)
import           Data.Int (Int64)
import           Data.Maybe.Unpacked
import Language.Haskell.TH

import PCG
import Types


asV3 :: Double -> V3
asV3 a = V3 a a a
{-# INLINE asV3 #-}


pointAtParameter :: V3 -> V3 -> Double -> V3
pointAtParameter !a !b !t =  a + (asV3 t) * b
{-# INLINE pointAtParameter #-}

unitVector :: V3 -> V3
unitVector !v = v / (asV3 $ v3len v)
{-# INLINE unitVector #-}

dot :: V3 -> V3 -> Double
dot !a !b = let (V3 x y z) = (a * b) in x + y + z
{-# INLINE dot #-}

cross :: V3 -> V3 -> V3
cross (V3 a1 a2 a3) (V3 b1 b2 b3) = V3 (a2 * b3 - a3 * b2)
                                       (-1 * (a1 * b3 - a3 * b1))
                                       (a1 * b2 - a2 * b1)
{-# INLINE cross #-}

v3len :: V3 -> Double
v3len v = sqrt $ dot v v
{-# INLINE v3len #-}

color :: (Hitable a) => Ray -> a -> PCG32 -> Int -> V3
color !r@(Ray o d) world gen callCount =
  let unitDirection@(V3 x _ _) = unitVector d
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


data HitResult = HitResult
  { t :: {-# UNPACK #-} !Double
  , p :: {-# UNPACK #-} !V3
  , normal :: {-# UNPACK #-} !V3
  , material :: !Material
  } deriving (Eq, Show, Ord)

instance Storable Sphere where
  sizeOf _ = $(return . LitE . IntegerL . fromIntegral $ sizeOf (undefined :: V3) + sizeOf (undefined :: Double) + sizeOf (undefined :: Material))
  alignment _ = 16 + (sizeOf (undefined :: Sphere)) `rem` 16
  {-# INLINE peekElemOff #-}
  peekElemOff addr idx =
    let elemAddr = addr `plusPtr` (idx * sizeOf (undefined :: Sphere))
        rAddr = elemAddr `plusPtr` sizeOf (undefined :: V3)
        mAddr = rAddr `plusPtr` sizeOf (undefined :: Double)
    in Sphere <$> peek elemAddr <*> peek rAddr <*> peek mAddr
  pokeElemOff addr idx elem@(Sphere c r m) = 
    let elemAddr = addr `plusPtr` (idx * sizeOf elem)
    in do
      poke elemAddr c
      poke (elemAddr `plusPtr` sizeOf c) r
      poke (elemAddr `plusPtr` sizeOf c `plusPtr` sizeOf r) m

newtype World a = World (Array B Ix1 a)

data Camera = Camera {
  originC :: {-# UNPACK #-} !V3
, lowerLeftCorner :: {-# UNPACK #-} !V3
, horizontal :: {-# UNPACK #-} !V3
, vertical :: {-# UNPACK #-} !V3
, u :: {-# UNPACK #-} !V3
, v :: {-# UNPACK #-} !V3
, w :: {-# UNPACK #-} !V3
, lensRadius :: {-# UNPACK #-} !Double
} deriving (Eq, Ord, Show)

makeCamera :: V3 -> V3 -> V3 -> Double -> Double -> Double -> Double -> Camera
makeCamera lookFrom lookAt vUp vfov aspect aperture focusDist =
  let theta = vfov * pi / 180
      halfHeight = tan $ theta / 2
      halfWidth = aspect * halfHeight
      w = unitVector $ lookFrom - lookAt
      u = unitVector $ cross vUp w
      v = cross w u

  in Camera lookFrom
            (lookFrom - (asV3 halfWidth) * (asV3 focusDist) * u - (asV3 halfHeight) * (asV3 focusDist) * v -  (asV3 focusDist) * w)
            (u * (asV3 $ 2 * focusDist * halfWidth))
            (v * (asV3 $ 2 * focusDist * halfHeight))
            u v w
            (aperture / 2)

getRay :: PCG32 -> Double -> Double -> Camera -> Ray
getRay gen s t cam = let (V3 x y _) = (asV3 $ lensRadius cam) * randomInUnitDisk gen
                         offset = (u cam) * (asV3 x) + (v cam) * (asV3  y)
                     in Ray (offset + originC cam)
                            ((lowerLeftCorner cam) + 
                             (asV3 s) * (horizontal cam) + 
                             (asV3 t) * (vertical cam) - 
                             (originC cam) - offset)
{-# INLINE getRay #-}

class Hitable a where
  hit :: Double -> Double -> a -> Ray -> Maybe HitResult

instance (Hitable a, Storable a) => Hitable (World a) where
  hit tMin tMax (World elems) !ray = A.foldlS combine Nothing elems
    where combine Nothing elem                      = hit tMin tMax elem ray
          combine (Just r@(HitResult t _ _ _)) elem = case hit tMin t elem ray of
            Nothing -> Just r
            Just res -> Just res
  {-# INLINE hit #-}

instance Hitable Sphere where
  hit tMin tMax (Sphere center r mat) (Ray origin direction) =
    if disc < 0
    then Nothing
    else let root = if negativeRoot < tMax && negativeRoot > tMin
                      then Just negativeRoot
                      else if positiveRoot < tMax && positiveRoot > tMin
                            then Just positiveRoot
                            else Nothing
         in root >>= \root -> 
                      let p = pointAtParameter origin direction root
                          normal = (p - center) / (asV3 r)
                      in Just $! HitResult root p normal mat
    where oc = origin - center
          a  = dot direction direction
          b = dot oc direction
          c = (dot oc oc) - r * r
          disc = b * b - a * c
          positiveRoot = (-b + (sqrt $ b*b - a*c)) / a
          negativeRoot = (-b - (sqrt $ b*b - a*c)) / a
  {-# INLINE hit #-}
  
reflect :: V3 -> V3 -> V3
reflect v n = v - asV3 (2 * dot v n) * n
{-# INLINE reflect #-}

scatter :: PCG32 -> Ray -> HitResult -> Maybe (V3, Ray)
scatter gen !ray@(Ray origin direction) (HitResult _ p normal (Lambertian albedo)) =
  let target = p + normal + randomInUnitSphere gen
      scattered = Ray p (target - p)
  in Just (albedo, scattered)

scatter gen !ray@(Ray origin direction) (HitResult _ p normal (Metal albedo fuzz)) =
  let reflected = reflect (unitVector direction) normal
      scattered = Ray p (reflected + (asV3 fuzz) * randomInUnitSphere gen)
  in if (dot reflected normal) > 0 then Just (albedo, scattered)
                                   else Nothing

scatter gen !ray@(Ray origin direction) (HitResult _ p normal (Dielectric refIdx)) =
  let reflected = reflect direction normal
      attenuation = asV3 1.0

      refract :: V3 -> V3 -> Double -> Maybe V3
      refract v n niOverNt = 
        let uv = unitVector v
            dt = dot uv n
            discriminant = 1.0 - niOverNt * niOverNt * (1 - dt * dt)
        in if discriminant > 0
           then Just $ (asV3 niOverNt) * (uv - n * asV3 dt) - n * (asV3 $ sqrt discriminant)
           else Nothing
      {-# INLINE refract #-}

      shlick :: Double -> Double -> Double
      shlick cosine refIdx = 
        let r0 = (1 - refIdx) / (1 + refIdx)
        in (r0 * r0) + (1 - r0 * r0) * (1 - cosine) ** 5
      {-# INLINE shlick #-}

      (outwardNormal, niOverNt, cosine) =
        if (dot direction normal) > 0
        then (-1 * normal, refIdx, refIdx * (dot direction normal) / v3len direction)
        else (normal, 1.0 / refIdx, -1 * (dot direction normal) / v3len direction)
  in case refract direction outwardNormal niOverNt of
    Just refracted -> let (r, _) = nextD gen
                      in if r < shlick cosine refIdx
                         then Just (attenuation, Ray p reflected)
                         else Just (attenuation, Ray p refracted)
    Nothing -> Just (attenuation, Ray p reflected)
{-# INLINE scatter #-}

randomInUnitSphere :: PCG32 -> V3
randomInUnitSphere gen = 
  let (v1, g1) = nextD gen
      (v2, g2) = nextD g1
      (v3, g3) = nextD g2
      p = asV3 2 * (V3 v1 v2 v3) - asV3 1
  in if dot p p > 1
     then randomInUnitSphere g3
     else p
{-# INLINE randomInUnitSphere #-}

randomInUnitDisk :: PCG32 -> V3
randomInUnitDisk gen = 
  let (v1, g1) = nextD gen
      (v2, g2) = nextD g1
      p = asV3 2 * (V3 v1 v2 0) - (V3 1 1 0)
  in if dot p p > 1
     then randomInUnitDisk g2
     else p
{-# INLINE randomInUnitDisk #-}



arrLightIx2 :: Ix2 -> World Sphere -> Int -> Image S RGB Double
arrLightIx2 arrSz@(sizeY :. sizeX) world samples =
  compute $ makeArrayR D Par arrSz lightFunc
  where lookFrom = V3 13 2 3
        lookAt = V3 0 0 0
        aperture = 0.1
        camera = makeCamera lookFrom lookAt 
                            (V3 0 1 0) 20
                            ((fromIntegral sizeX) / (fromIntegral sizeY))
                            aperture
                            10
        samplesD = fromIntegral samples

        castRay :: Int -> Int -> V3 -> Int -> V3
        castRay !i !j !rgb !sample =
          let 
              idx = 2 * (i + 1) * (j + 1) * (sample + 1)
              stdGen = newPCG32 (fromIntegral idx) (fromIntegral idx)
              (r1, g1) = nextD stdGen
              (r2, g2) = nextD g1
              (g3, g4) = split g2
              u = (r1 + fromIntegral i) / fromIntegral sizeX
              v = (r2 + fromIntegral (sizeY - 1 - j)) / fromIntegral sizeY

              ray = getRay g3 u v camera
              col = color ray world g4 0
          in rgb + col
        lightFunc arrI@(j :. i) = 
          let (V3 r g b) = L.foldl' (castRay i j) 0 [0..(samples - 1)]
          in PixelRGB (sqrt $ r / samplesD) (sqrt $ g / samplesD) (sqrt $ b / samplesD)
        {-# INLINE lightFunc #-}
{-# INLINE arrLightIx2 #-}

randDoubles :: PCG32 -> Int -> ([Double], PCG32)
randDoubles gen howMany = L.foldl' genDouble ([], gen) [1..howMany]
  where genDouble (elems, gen') _ = let (x, gen'') = nextD gen'
                                    in (x:elems, gen'')

generateWorld :: PCG32 -> World Sphere
generateWorld gen = 
  let initialSpheres = [
        (Sphere (V3 0 (-1000) 0) 1000 (Lambertian (V3 0.5 0.5 0.5)))
        , (Sphere (V3 0 1 0) 1 (Dielectric 1.5))
        , (Sphere (V3 (-4) 1 0) 1 (Lambertian (V3 0.4 0.2 0.1)))
        , (Sphere (V3 4 1 0) 1 (Metal (V3 0.7 0.6 0.5) 0))
        ]

      genElem :: (PCG32, [Sphere]) -> (Double, Double) -> (PCG32, [Sphere])
      genElem (g, world) (a, b) = 
        let (material, g2) = nextD g
            (x, g3) = nextD g2
            (z, g4) = nextD g3
            center = V3 (a + 0.9 * x) 0.2 (b + 0.9 * z)
        in if (v3len $ center - (V3 4 0.2 0)) < 0.9
           then (g4, world)
           else case material of
            x | x < 0.8 -> let (nums, g5) = randDoubles g4 6
                           in (g5, (Sphere center 0.2 (Lambertian 
                                    (V3 (nums !! 0 * nums !! 1) 
                                        (nums !! 2 * nums !! 3) 
                                        (nums !! 4 * nums !! 5)))):world)
            x | x < 0.95 -> let (nums, g5) = randDoubles g4 4
                            in (g5, (Sphere center 0.2 
                                      (Metal (V3 (0.5 * (1 + nums !! 0))
                                               (0.5 * (1 + nums !! 1))
                                               (0.5 * (1 + nums !! 2)))
                                           (0.5 * nums !! 3))):world)
            _ -> (g4, (Sphere center 0.2 (Dielectric 1.5)):world) 

      randomSpheres = L.foldl' genElem (gen, [])
                      [(fromIntegral x, fromIntegral y) | x <- [-11..11], y <- [-11..11]] 
  in World $ fromList Seq (initialSpheres ++ snd randomSpheres)
      

someFunc :: IO ()
someFunc = do          
  let stdGen = newPCG32 1337 1337
      world = generateWorld stdGen
      img = arrLightIx2 (100 :. 200) world 100
  writeImage "light.png" img
