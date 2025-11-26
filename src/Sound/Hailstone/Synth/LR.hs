{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- for an MArray instance
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Stereo pair type, with specialized SIMD representations.
--
-- Inspired by the type family approach of
-- <https://github.com/mikeizbicki/simd/blob/master/src/Data/SIMD/SIMD2.hs>
module Sound.Hailstone.Synth.LR
( LR
, mkLR, withLR, dupLR, hsumLR, mapLR, liftA2LR, zeroLR, (.+:), (.*:), balanceLR
)
where

import Data.Kind (Type)
import Data.Int (Int16, Int64)
import Foreign (Storable, castPtr, pokeElemOff, peekElemOff, sizeOf, alignment)

import qualified GHC.Base as B
import qualified GHC.Int as I
import GHC.Exts (Ptr(..))

-- for an MArray instance
import Data.Array.Base
  ( MArray, STUArray(..), UArray(..), IArray
  , safe_scale, getBounds, newArray, newArray_, getNumElements
  , unsafeNewArray_, unsafeNewArraySTUArray_, unsafeRead, unsafeWrite
  , bounds, numElements, unsafeArray, unsafeAt, unsafeReplace, unsafeAccum, unsafeAccumArray
  , unsafeArrayUArray, unsafeAccumUArray, unsafeAccumArrayUArray, unsafeReplaceUArray
  )
import Data.Array.IO.Internals (IOUArray(..))
import GHC.ST (ST(..))
import Control.Monad.ST (stToIO, runST)

class LRx a where
  -- | A stereo pair of values of type @a@. Has special SIMD representations for certain @a@
  -- types. (Only `Double`, `Int16` at the moment (our real value type and sample out type.)
  data LR a :: Type
  -- this is weird but this is how you'd add to a class an associated type whose kind is not
  -- Type (a.k.a. TYPE LiftedRep), but that of an unboxed type (e.g. TYPE DoubleRep), making
  -- the choice of RuntimeRep a \"kind family\" indexed by the instance type.
  type PRep a :: B.RuntimeRep
  type PT a (rep :: B.RuntimeRep) :: B.TYPE rep

  -- | Create a stereo pair from two values.
  mkLR :: a -> a -> LR a
  -- | Broadcast a single value into a stereo pair.
  dupLR :: a -> LR a
  -- | Run a function on the left and right values.
  withLR :: LR a -> (a -> a -> b) -> b
  -- | A zero value. (This is only an `LRx` method because @LRxNum Int16@ can't compile with
  -- GHC NCG yet (9.12.2) otherwise we'd be using the num instance we get from that. We just
  -- need a zero value.)
  zeroLR :: LR a
  -- | Horizontal sum. Also in `LRx` instead of `LRxNum` for the same reason as `zeroLR`.
  hsumLR :: LR a -> a

  -- these two methods must be defined with an explicit concrete type sig at each instance!
  -- otherwise GHC can't figure out the runtime representation of the passed-in func's args.

  -- | Map using a function on the primitive type, not the boxed type.
  mapLRPrim :: (PT a (PRep a) -> PT a (PRep a)) -> LR a -> LR a

  -- | liftA2 using a function on the primitive type, not the boxed type.
  liftA2LRPrim :: (PT a (PRep a) -> PT a (PRep a) -> PT a (PRep a)) -> LR a -> LR a -> LR a

class (Num a, LRx a) => LRxNum a where
  plusLR :: LR a -> LR a -> LR a
  minusLR :: LR a -> LR a -> LR a
  timesLR :: LR a -> LR a -> LR a
  negLR :: LR a -> LR a
  shiftLR :: a -> LR a -> LR a
  scaleLR :: a -> LR a -> LR a

-- | Add a value to both sides of the stereo pair.
(.+:) :: (LRxNum a) => a -> LR a -> LR a
(.+:) = shiftLR
{-# INLINE (.+:) #-}

-- | Multiply a value onto both sides of the stereo pair.
(.*:) :: (LRxNum a) => a -> LR a -> LR a
(.*:) = scaleLR
{-# INLINE (.*:) #-}

class (Fractional a, LRx a, LRxNum a) => LRxFrac a where
  divLR :: LR a -> LR a -> LR a
  -- | Rebalance a stereo pair with a pan value (0 = hard left, 1 = hard right, 0.5 center).
  balanceLR :: a -> LR a -> LR a

--------------------------------------------------------------------------------
-- ** Free traits for any `LR`

-- | Unfortunately we can't write this as an `fmap` in a real `Functor` instance, due to the
-- `LRx` constraint. Map using a function on the boxed type.
mapLR :: (LRx a, LRx b) => (a -> b) -> LR a -> LR b
mapLR f lr = withLR lr $ \al ar -> mkLR (f al) (f ar)

-- | Lift a pure function to work on stereo pairs.
liftA2LR :: (LRx a, LRx b, LRx x) => (a -> b -> x) -> LR a -> LR b -> LR x
liftA2LR f lra lrb = withLR lra $ \al ar -> withLR lrb $ \bl br -> mkLR (f al bl) (f ar br)

instance (LRxNum a) => Num (LR a) where
  (+) = plusLR
  (*) = timesLR
  (-) = minusLR
  negate = negLR
  fromInteger n = dupLR (fromInteger n)
  -- we can actually add bespoke methods for these? but there's no abs simd prim in ghc yet
  abs = mapLR abs
  signum = mapLR signum

  {-# INLINE (+) #-}
  {-# INLINE (*) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}

instance (LRxFrac a) => Fractional (LR a) where
  (/) = divLR
  fromRational n = dupLR (fromRational n)

  {-# INLINE (/) #-}
  {-# INLINE fromRational #-}

--------------------------------------------------------------------------------
-- ** Instance for an arbitrary type in the LR pair

-- | for any type we wish to put in an LR that doesn't have a dedicated implementation
newtype LRThing a = MkLRThing a
  deriving (Num, Eq, Ord, Bounded, Enum, Real, Integral, Fractional, Storable)

-- | The `LRx` instance for an arbitrary (`LRThing`-wrapped) type is just a strict tuple.
instance (Num a) => LRx (LRThing a) where
  data LR (LRThing a) = MkLR !a !a
  type PRep (LRThing a) = B.LiftedRep
  type PT (LRThing a) B.LiftedRep = a
  mkLR (MkLRThing al) (MkLRThing ar) = MkLR al ar
  dupLR (MkLRThing a) = MkLR a a
  withLR (MkLR al ar) f = f (MkLRThing al) (MkLRThing ar)
  zeroLR = (MkLR 0 0)
  hsumLR (MkLR al ar) = MkLRThing (al + ar)

  mapLRPrim :: (a -> a) -> LR (LRThing a) -> LR (LRThing a)
  mapLRPrim f (MkLR al ar) = MkLR (f al) (f ar)

  liftA2LRPrim :: (a -> a -> a) -> LR (LRThing a) -> LR (LRThing a) -> LR (LRThing a)
  liftA2LRPrim f (MkLR al ar) (MkLR bl br) = MkLR (f al bl) (f ar br)

  {-# INLINE mkLR #-}
  {-# INLINE dupLR #-}
  {-# INLINE withLR #-}
  {-# INLINE zeroLR #-}
  {-# INLINE hsumLR #-}
  {-# INLINE mapLRPrim #-}
  {-# INLINE liftA2LRPrim #-}

instance (Num a) => LRxNum (LRThing a) where
  plusLR (MkLR al ar) (MkLR bl br) = MkLR (al + bl) (ar + br)
  minusLR (MkLR al ar) (MkLR bl br) = MkLR (al - bl) (ar - br)
  timesLR (MkLR al ar) (MkLR bl br) = MkLR (al * bl) (ar * br)
  negLR (MkLR al ar) = MkLR (negate al) (negate ar)
  shiftLR (MkLRThing a) (MkLR al ar) = MkLR (a + al) (a + ar)
  scaleLR (MkLRThing a) (MkLR al ar) = MkLR (a * al) (a * ar)

  {-# INLINE plusLR #-}
  {-# INLINE timesLR #-}
  {-# INLINE minusLR #-}
  {-# INLINE negLR #-}
  {-# INLINE shiftLR #-}
  {-# INLINE scaleLR #-}

instance (Num a, Storable a) => Storable (LR (LRThing a)) where
  sizeOf = const $ 2 * sizeOf (undefined :: a)
  alignment = const $ alignment (undefined :: a)
  peekElemOff ptr idx = do
    q <- pure $ castPtr ptr
    al <- peekElemOff q idx
    ar <- peekElemOff q (idx + 1)
    pure (MkLR al ar)
  pokeElemOff ptr idx (MkLR al ar)  = do
    q <- pure $ castPtr ptr
    pokeElemOff q idx al
    pokeElemOff q (idx + 1) ar

  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE peekElemOff #-}
  {-# INLINE pokeElemOff #-}

instance (Fractional a, Ord a) => LRxFrac (LRThing a) where
  divLR (MkLR al ar) (MkLR bl br) = MkLR (al / bl) (ar / br)
  balanceLR (MkLRThing p) (MkLR al ar) = if p <= 0.5
    then MkLR al (2 * p * ar)
    else MkLR (2 * (1 - p) * al) ar

  {-# INLINE divLR #-}
  {-# INLINE balanceLR #-}

--------------------------------------------------------------------------------
-- ** SIMD representations of LR T

-- | The @`LR` `Double`@ representation is a 2-double SIMD vector.
instance LRx Double where
  data LR Double = MkDX2# B.DoubleX2#
  type PT Double B.DoubleRep = B.Double#
  type PRep Double = B.DoubleRep
  mkLR (B.D# al) (B.D# ar) = MkDX2# (B.packDoubleX2# (# al, ar #))
  dupLR (B.D# a) = MkDX2# (B.broadcastDoubleX2# a)
  withLR (MkDX2# lr) f = let !(# al, ar #) = B.unpackDoubleX2# lr in f (B.D# al) (B.D# ar)
  zeroLR = MkDX2# (B.broadcastDoubleX2# 0.0##)
  hsumLR (MkDX2# lr) = let !(# al, ar #) = B.unpackDoubleX2# lr in B.D# (al B.+## ar)

  -- these type sigs are absolutely necessary, otherwise GHC can't infer the runtime repr of
  -- these functions (it can't evaluate thru the PT Double (PRep Double)) to get Double#)
  mapLRPrim :: (B.Double# -> B.Double#) -> LR Double -> LR Double
  mapLRPrim f (MkDX2# lr) = let !(# al, ar #) = B.unpackDoubleX2# lr
    in MkDX2# (B.packDoubleX2# (# f al, f ar #))

  liftA2LRPrim :: (B.Double# -> B.Double# -> B.Double#) -> LR Double -> LR Double -> LR Double
  liftA2LRPrim f (MkDX2# lra) (MkDX2# lrb) = let
    !(# al, ar #) = B.unpackDoubleX2# lra
    !(# bl, br #) = B.unpackDoubleX2# lrb
    in MkDX2# (B.packDoubleX2# (# f al bl, f ar br #))

  {-# INLINE mkLR #-}
  {-# INLINE dupLR #-}
  {-# INLINE withLR #-}
  {-# INLINE zeroLR #-}
  {-# INLINE hsumLR #-}
  {-# INLINE mapLRPrim #-}
  {-# INLINE liftA2LRPrim #-}

instance LRxNum Double where
  plusLR (MkDX2# a) (MkDX2# b) = MkDX2# (B.plusDoubleX2# a b)
  minusLR (MkDX2# a) (MkDX2# b) = MkDX2# (B.minusDoubleX2# a b)
  timesLR (MkDX2# a) (MkDX2# b) = MkDX2# (B.timesDoubleX2# a b)
  negLR (MkDX2# a) = MkDX2# (B.negateDoubleX2# a)
  shiftLR (B.D# a) (MkDX2# lr) = MkDX2# (B.plusDoubleX2# (B.broadcastDoubleX2# a) lr)
  scaleLR (B.D# a) (MkDX2# lr) = MkDX2# (B.timesDoubleX2# (B.broadcastDoubleX2# a) lr)

  {-# INLINE plusLR #-}
  {-# INLINE timesLR #-}
  {-# INLINE minusLR #-}
  {-# INLINE negLR #-}
  {-# INLINE shiftLR #-}
  {-# INLINE scaleLR #-}

instance LRxFrac Double where
  divLR (MkDX2# a) (MkDX2# b) = MkDX2# (B.divideDoubleX2# a b)
  -- we turn the branch into multiplying by 1 or 0, no simd unpacking needed
  balanceLR (B.D# p) (MkDX2# lr) = let
    tru = B.int2Double# ( p B.<=## 0.5## )
    fls = 1.0## B.-## tru
    mult = B.packDoubleX2#
      (# tru                        B.+## fls B.*## (2.0## B.*## (1.0## B.-## p))
      ,  tru B.*## (2.0## B.*## p)  B.+## fls                                  #)
      -- if p <= 0.5
      -- then MkLR al (2 * p * ar)
      -- else MkLR (2 * (1 - p) * al) ar
    in MkDX2# (B.timesDoubleX2# mult lr)

instance Storable (LR Double) where
  sizeOf = const $ 2 * sizeOf (undefined :: Double)
  alignment = const $ alignment (undefined :: Double)
  peekElemOff (Ptr addr) (B.I# idx) = B.IO $ \s ->
    case B.readDoubleOffAddrAsDoubleX2# addr (2# B.*# idx) s of
      (# s', lr #) -> (# s', MkDX2# lr #)
  pokeElemOff (Ptr addr) (B.I# idx) (MkDX2# lr) = B.IO $ \s ->
    case B.writeDoubleOffAddrAsDoubleX2# addr (2# B.*# idx) lr s of
      s' -> (# s', () #)

  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE peekElemOff #-}
  {-# INLINE pokeElemOff #-}

-- | adapted from <https://hackage.haskell.org/package/array-0.5.8.0/docs/src/Data.Array.Base.html>
instance MArray (STUArray s) (LR Double) (ST s) where
  {-# INLINE getBounds #-}
  getBounds (STUArray l u _ _) = return (l,u)
  {-# INLINE getNumElements #-}
  getNumElements (STUArray _ _ n _) = return n
  {-# INLINE unsafeNewArray_ #-}
  unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u)
    (safe_scale (case (2 * sizeOf (undefined :: Double)) of (I.I# sz) -> sz))
  {-# INLINE newArray_ #-}
  newArray_ arrBounds = newArray arrBounds 0
  {-# INLINE unsafeRead #-}
  unsafeRead (STUArray _ _ _ marr#) (I.I# i#) = ST $ \s1# ->
    case B.readDoubleX2Array# marr# i# s1# of (# s2#, e# #) -> (# s2#, MkDX2# e# #)
  {-# INLINE unsafeWrite #-}
  unsafeWrite (STUArray _ _ _ marr#) (I.I# i#) (MkDX2# e#) = ST $ \s1# ->
    case B.writeDoubleX2Array# marr# i# e# s1# of s2# -> (# s2#, () #)

instance MArray IOUArray (LR Double) IO where
  {-# INLINE getBounds #-}
  getBounds (IOUArray arr) = stToIO $ getBounds arr
  {-# INLINE getNumElements #-}
  getNumElements (IOUArray arr) = stToIO $ getNumElements arr
  {-# INLINE newArray #-}
  newArray lu initialValue = stToIO $ do
      marr <- newArray lu initialValue; return (IOUArray marr)
  {-# INLINE unsafeNewArray_ #-}
  unsafeNewArray_ lu = stToIO $ do
      marr <- unsafeNewArray_ lu; return (IOUArray marr)
  {-# INLINE newArray_ #-}
  newArray_ = unsafeNewArray_
  {-# INLINE unsafeRead #-}
  unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
  {-# INLINE unsafeWrite #-}
  unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance IArray UArray (LR Double) where
  {-# INLINE bounds #-}
  bounds (UArray l u _ _) = (l,u)
  {-# INLINE numElements #-}
  numElements (UArray _ _ n _) = n
  {-# INLINE unsafeArray #-}
  unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)
  {-# INLINE unsafeAt #-}
  unsafeAt (UArray _ _ _ arr#) (I.I# i#) = MkDX2# (B.indexDoubleX2Array# arr# i#)
  {-# INLINE unsafeReplace #-}
  unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
  {-# INLINE unsafeAccum #-}
  unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
  {-# INLINE unsafeAccumArray #-}
  unsafeAccumArray f initialValue lu ies = runST (unsafeAccumArrayUArray f initialValue lu ies)


instance Floating (LR Double) where
  pi = dupLR pi
  exp = mapLRPrim B.expDouble#
  log = mapLRPrim B.logDouble#
  sin = mapLRPrim B.sinDouble#
  cos = mapLRPrim B.cosDouble#
  sqrt = mapLRPrim B.sqrtDouble#
  asin = mapLRPrim B.asinDouble#
  acos = mapLRPrim B.acosDouble#
  atan = mapLRPrim B.atanDouble#
  sinh = mapLRPrim B.sinhDouble#
  cosh = mapLRPrim B.coshDouble#
  asinh = mapLRPrim B.asinhDouble#
  acosh = mapLRPrim B.acoshDouble#
  atanh = mapLRPrim B.atanhDouble#
  (**) = liftA2LRPrim (B.**##)

-- right now the Int16 instance is kind of useless because we don't do any math on it, aside
-- from clamping to the Int16 bounds, which we do on the boxed ints anyway. it's really only
-- here because LRThing is too annoying to work with for non-specialized impl LR types.

int16ToInt64# :: B.Int16# -> B.Int64#
int16ToInt64# n = B.intToInt64# (B.int16ToInt# n)
{-# INLINE int16ToInt64# #-}

int64ToInt16# :: B.Int64# -> B.Int16#
int64ToInt16# n = B.intToInt16# (B.int64ToInt# n)
{-# INLINE int64ToInt16# #-}

instance LRx Int16 where
  data LR Int16 = MkI16X2# B.Int64X2#
  type PT Int16 B.Int16Rep = B.Int16#
  type PRep Int16 = B.Int16Rep
  mkLR (I.I16# al) (I.I16# ar) = MkI16X2# (B.packInt64X2# (# (int16ToInt64# al), (int16ToInt64# ar) #))
  dupLR (I.I16# a) = MkI16X2# (B.broadcastInt64X2# (int16ToInt64# a))
  withLR (MkI16X2# lr) f = let !(# al, ar #) = B.unpackInt64X2# lr in f (I.I16# (int64ToInt16# al)) (I.I16# (int64ToInt16# ar))
  zeroLR = MkI16X2# (B.broadcastInt64X2# (case (0 :: Int64) of (I.I64# n) -> n))
  hsumLR (MkI16X2# lr) = let !(# al, ar #) = B.unpackInt64X2# lr in I.I16# (B.plusInt16# (int64ToInt16# al) (int64ToInt16# ar))

  mapLRPrim :: (B.Int16# -> B.Int16#) -> LR Int16 -> LR Int16
  mapLRPrim f (MkI16X2# lr) = let
    !(# al, ar #) = B.unpackInt64X2# lr
    al16 = int64ToInt16# al
    ar16 = int64ToInt16# ar
    xl = int16ToInt64# (f al16)
    xr = int16ToInt64# (f ar16)
    in MkI16X2# (B.packInt64X2# (# xl, xr #))

  liftA2LRPrim :: (B.Int16# -> B.Int16# -> B.Int16#) -> LR Int16 -> LR Int16 -> LR Int16
  liftA2LRPrim f (MkI16X2# lra) (MkI16X2# lrb) = let
    !(# al, ar #) = B.unpackInt64X2# lra
    !(# bl, br #) = B.unpackInt64X2# lrb
    al16 = int64ToInt16# al
    ar16 = int64ToInt16# ar
    bl16 = int64ToInt16# bl
    br16 = int64ToInt16# br
    xl = int16ToInt64# (f al16 bl16)
    xr = int16ToInt64# (f ar16 br16)
    in MkI16X2# (B.packInt64X2# (# xl, xr #))

  {-# INLINE mkLR #-}
  {-# INLINE dupLR #-}
  {-# INLINE withLR #-}
  {-# INLINE zeroLR #-}
  {-# INLINE hsumLR #-}
  {-# INLINE mapLRPrim #-}
  -- liftA2LRPrim as of 9.12.2 compiles to bad asm if i inline; seems like 9.12.3 might fix
  -- {-# INLINE liftA2LRPrim #-}


-- GHC 9.12.2 native code gen can't compile these calls yet, deferring to LLVM (which I am
-- not running..)

-- instance LRxNum Int16 where
--   plusLR (MkI16X2# a) (MkI16X2# b) = MkI16X2# (B.plusInt64X2# a b)
--   minusLR (MkI16X2# a) (MkI16X2# b) = MkI16X2# (B.minusInt64X2# a b)
--   timesLR (MkI16X2# a) (MkI16X2# b) = MkI16X2# (B.timesInt64X2# a b)
--   negLR (MkI16X2# a) = MkI16X2# (B.negateInt64X2# a)
--   shiftLR (I.I16# a) (MkI16X2# lr) = MkI16X2# (B.plusInt64X2# (B.broadcastInt64X2# (int16ToInt64# a)) lr)
--   scaleLR (I.I16# a) (MkI16X2# lr) = MkI16X2# (B.timesInt64X2# (B.broadcastInt64X2# (int16ToInt64# a)) lr)

--   {-# INLINE plusLR #-}
--   {-# INLINE timesLR #-}
--   {-# INLINE minusLR #-}
--   {-# INLINE negLR #-}
--   {-# INLINE shiftLR #-}
--   {-# INLINE scaleLR #-}
