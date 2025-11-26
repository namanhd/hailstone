{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}
-- for an MArray instance
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Sound.Hailstone.Sequencing.Now where
import Sound.Hailstone.Synth.SynthVal

-- for an MArray instance
import Data.Array.Base
  ( MArray, STUArray(..)
  , safe_scale, getBounds, newArray, newArray_, getNumElements
  , unsafeNewArray_, unsafeNewArraySTUArray_, unsafeRead, unsafeWrite
  )

import Data.Array.IO.Internals (IOUArray(..))
import GHC.ST (ST(..))
import Control.Monad.ST (stToIO)
import qualified GHC.Int as I
import qualified GHC.Base as B
import Foreign (sizeOf)

-- | A `Now` is what an instrument receives from a `Node` to play a `Cell`.
--
-- Varying frequency or amplitude (per-note portamento, vibrato, or volume slides) specified
-- by `Cell`'s `MkC` (non-articulated) or `MkAC` (articulated) constructors are reflected as
-- time-varying `Now` sent to the instrument upon every new sample. We can add more metadata
-- here, such as modulation knobs that can be mapped to any parameter in the instrument.
data Now = MkNow
  { freq :: !Freq -- ^ current frequency
  , ampl :: !Ampl -- ^ current amplitude
  , pan :: !Pan -- ^ current pan
  , env :: !SynthVal -- ^ current value of the envelope
  } deriving (Show, Eq)


-- | adapted from <https://hackage.haskell.org/package/array-0.5.8.0/docs/src/Data.Array.Base.html>
instance MArray (STUArray s) Now (ST s) where
  {-# INLINE getBounds #-}
  getBounds (STUArray l u _ _) = return (l,u)
  {-# INLINE getNumElements #-}
  getNumElements (STUArray _ _ n _) = return n
  {-# INLINE unsafeNewArray_ #-}
  unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u)
    (safe_scale (case (4 * sizeOf (undefined :: Double)) of (I.I# sz) -> sz))
  {-# INLINE newArray_ #-}
  newArray_ arrBounds = newArray arrBounds (MkNow 0.0 0.0 0.0 0.0)
  {-# INLINE unsafeRead #-}
  unsafeRead (STUArray _ _ _ marr#) (I.I# i#) = let ii# = 4# B.*# i# in ST $ \s1# ->
    case B.readDoubleArray# marr# (ii#        ) s1# of { (# s2#, e1# #) ->
    case B.readDoubleArray# marr# (ii# B.+# 1#) s2# of { (# s3#, e2# #) ->
    case B.readDoubleArray# marr# (ii# B.+# 2#) s3# of { (# s4#, e3# #) ->
    case B.readDoubleArray# marr# (ii# B.+# 3#) s4# of { (# s5#, e4# #) ->
      (# s5#, MkNow (B.D# e1#) (B.D# e2#) (B.D# e3#) (B.D# e4#)  #) }}}}
  {-# INLINE unsafeWrite #-}
  unsafeWrite (STUArray _ _ _ marr#) (I.I# i#) (MkNow (B.D# e1#) (B.D# e2#) (B.D# e3#) (B.D# e4#)) = let ii# = 4# B.*# i# in ST $ \s1# ->
    case B.writeDoubleArray# marr# (ii#        ) e1# s1# of { s2# -> 
    case B.writeDoubleArray# marr# (ii# B.+# 1#) e2# s2# of { s3# -> 
    case B.writeDoubleArray# marr# (ii# B.+# 2#) e3# s3# of { s4# -> 
    case B.writeDoubleArray# marr# (ii# B.+# 3#) e4# s4# of { s5# -> 
      (# s5#, () #) }}}}

instance MArray IOUArray Now IO where
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
