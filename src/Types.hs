{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Types where
import           Foreign.Storable
import qualified Foreign.Storable.Record as Store
import           Foreign.Ptr (Ptr(..), plusPtr, castPtr)
import           Data.Int (Int64)
import           Control.DeepSeq (force, deepseq, NFData)
import           GHC.Generics (Generic)
import           Control.Monad (liftM)

import           Data.Word (Word64)
import           Data.Binary.IEEE754 (doubleToWord, wordToDouble)

{-import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed.Base as U
import qualified Data.Vector.Primitive as P-}
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import           Data.Tuple.HT (uncurry3)

data V3 = V3 {-# UNPACK #-} !Double  {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show, Eq, Ord, Generic)

instance NFData V3

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

derivingUnbox "V3"
  [t| V3 -> (Double, Double, Double) |]
  [| \(V3 x y z) -> (x, y, z) |]
  [| uncurry3 V3 |]

instance Storable V3 where
  sizeOf _    = 24
  alignment _ = 32
  peekElemOff !addr !idx =
    let elemAddr = addr `plusPtr` (idx * sizeOf (undefined :: V3))
    in do
      v1 <- peek elemAddr
      v2 <- peek (elemAddr `plusPtr` 8)
      v3 <- peek (elemAddr `plusPtr` 16)
      return $ V3 v1 v2 v3
  pokeElemOff !addr !idx !elem@(V3 v1 v2 v3) = 
    let elemAddr = addr `plusPtr` (idx * sizeOf elem)
    in do
      poke elemAddr v1
      poke (elemAddr `plusPtr` 8) v2
      poke (elemAddr `plusPtr` 16) v3

data Material
  = Metal { albedo :: {-# UNPACK #-} !V3, fuzz :: {-# UNPACK #-} !Double }
  | Lambertian { albedo :: {-# UNPACK #-} !V3 }
  | Dielectric { refIndex :: {-# UNPACK #-} !Double }
  deriving (Show, Eq, Ord, Generic)

instance NFData Material

derivingUnbox "Material"
  [t| Material -> (Int64, V3, Double) |]
  [| \case
      Metal albedo fuzz -> (0, albedo, fuzz)
      Lambertian albedo -> (1, albedo, 0.0)
      Dielectric refIdx -> (2, (V3 0 0 0), refIdx) |]
  [| \case
      (0, albedo, fuzz) -> Metal albedo fuzz
      (1, albedo, _) -> Lambertian albedo
      (2, _, refIdx) -> Dielectric refIdx |]

instance Storable Material where
  sizeOf _    = 40
  alignment _ = 48
  peekElemOff !addr !idx = 
    let elemAddr = addr `plusPtr` (idx * sizeOf (undefined :: Material))
        dataBeginAddr = elemAddr `plusPtr` 8
    in do
      constructorId <- peek (castPtr elemAddr :: Ptr Int64)
      case constructorId of
        0 -> Metal <$> peek dataBeginAddr <*> peek (dataBeginAddr `plusPtr` 24)
        1 -> Lambertian <$> peek (castPtr dataBeginAddr)
        2 -> Dielectric <$> peek (castPtr dataBeginAddr)
  pokeElemOff !addr !idx !elem =
    let elemAddr = addr `plusPtr` (idx * sizeOf elem)
        dataBeginAddr = elemAddr `plusPtr` 8
    in case elem of
      Metal albedo fuzz -> do
        poke elemAddr (0 :: Int64)
        poke dataBeginAddr albedo
        poke (dataBeginAddr `plusPtr` 24) fuzz
      Lambertian albedo -> do
        poke elemAddr (1 :: Int64)
        poke dataBeginAddr albedo
      Dielectric refIndex -> do
        poke elemAddr (2 :: Int64)
        poke (castPtr dataBeginAddr) refIndex

data Sphere = Sphere
  { center :: {-# UNPACK #-} !V3
  , radius :: {-# UNPACK #-} !Double
  , material :: Material
  } deriving (Eq, Show, Ord, Generic)

instance Storable Sphere where
  sizeOf _ = sizeOf (undefined :: V3) + sizeOf (undefined :: Double) + sizeOf (undefined :: Material)
  alignment _ = 16 + (sizeOf (undefined :: Sphere)) `rem` 16
  peek !elemAddr =
    let rAddr = castPtr $ elemAddr `plusPtr` sizeOf (undefined :: V3)
        mAddr = castPtr $ rAddr `plusPtr` sizeOf (undefined :: Double)
    in Sphere <$> peek (castPtr elemAddr) <*> peek rAddr <*> peek mAddr
  poke !elemAddr !elem@(Sphere c r m) = 
    do
      poke (castPtr elemAddr) c
      poke (castPtr $ elemAddr `plusPtr` sizeOf c) r
      poke (castPtr $ elemAddr `plusPtr` sizeOf c `plusPtr` sizeOf r) m

derivingUnbox "Sphere"
  [t| Sphere -> (V3, Double, Material)|]
  [| \(Sphere c r m) -> (c, r, m) |]
  [| \(c, r, m) -> Sphere c r m |]

instance NFData Sphere

data Ray = Ray
  { origin    :: {-# UNPACK #-} !V3
  , direction :: {-# UNPACK #-} !V3
  } deriving (Eq, Show, Generic)

instance NFData Ray

