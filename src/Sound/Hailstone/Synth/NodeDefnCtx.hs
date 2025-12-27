{-# LANGUAGE DerivingVia #-}

module Sound.Hailstone.Synth.NodeDefnCtx where
import Sound.Hailstone.Synth.SynthVal
import Control.Monad.Reader

-- | Carries @MonadHas*@ abilities needed at node definition time (as opposed to execution).
-- Other modules may write new classes and instances to add definition-time functionality.
newtype NodeDefnCtx a = MkNodeDefnCtx { runNodeDefnCtx :: TimeVal -> IO a }
  deriving (Functor, Applicative, Monad, MonadReader TimeVal, MonadIO) via ReaderT TimeVal IO

class Monad m => MonadHasDeltaTime m where
  askDeltaTime :: m TimeVal

instance MonadHasDeltaTime NodeDefnCtx where
  askDeltaTime = ask
