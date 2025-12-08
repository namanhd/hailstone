{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}

module Sound.Hailstone.Synth.NodeDefnCtx where

import Sound.Hailstone.Synth.SynthVal
import Data.IORef
import Control.Monad.Reader

newtype NodeDefnCtx a = MkNodeDefnCtx { runNodeDefnCtx :: TimeVal -> IO a }
  deriving (Functor, Applicative, Monad, MonadReader TimeVal, MonadIO) via ReaderT TimeVal IO

class Monad m => MonadHasDeltaTime m where
  askDeltaTime :: m TimeVal

class Monad m => MonadHasSharing m where
  newShareRef :: a -> m (IORef a)

instance MonadHasSharing NodeDefnCtx where
  newShareRef = liftIO . newIORef

instance MonadHasDeltaTime NodeDefnCtx where
  askDeltaTime = ask