{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Sound.Hailstone.Sequencing.Cell
( Cell(..), ACell(..), promoteCtoAC', lowerAC'toAC )
where

import Sound.Hailstone.Synth.Node

data Cell e
  = MkC -- ^ non-articulated cell specification
    { freq :: !Freq -- ^Frequency of the note
    , ampl :: !Ampl -- ^Amplitude of the note as a linear multiplier (should be between 0 and 1)
    , start :: !TimeVal -- ^Start time of the note in seconds.
    , dur :: !TimeVal
    -- ^Duration of the note in seconds, which can differ from `adsrTotalTime`, but whether it
    -- or the ADSR total time gets used as the time at which the note is cut is configured via
    -- an argument to `retriggerWith`.
    , pan :: !Pan
    -- ^Pan of a note, 0 is hard left, 0.5 is center, 1 is hard right.
    , adsr :: !ADSRParams
    -- ^ The envelope settings for this note.
    }
  | MkAC -- ^ articulated cell specification
    { freqs :: !(Node e Freq)
    , ampls :: !(Node e Ampl)
    , start :: !TimeVal
    , dur :: !TimeVal
    , pans :: !(Node e Pan)
    , env :: !(Node e Percent)
    }

data ACell e = MkAC'
  { freqs' :: !(Node e Freq)
  , ampls' :: !(Node e Ampl)
  , start' :: !TimeVal
  , dur' :: !TimeVal
  , pans' :: !(Node e Pan)
  , env' :: !(Node e Percent)
  }

-- | for debug only!
instance Show (Cell e) where
  show (MkC f a s d p e) = "MkC(" ++ show f ++ ", " ++ show a ++ ", " ++ show s ++ ", " ++ show d ++ ", " ++show p ++ ", "++ show e ++ ")"
  show (MkAC {}) = "MkAC <cannot show>"

promoteCtoAC' :: Cell e -> ACell e
promoteCtoAC' (MkC f a s d p e) = MkAC' (pure f) (pure a) s d (pure p) (adsr' e)
promoteCtoAC' (MkAC f a s d p e) = MkAC' f a s d p e

lowerAC'toAC :: ACell e -> Cell e
lowerAC'toAC (MkAC' f a s d p e) = MkAC f a s d p e