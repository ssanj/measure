{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Model where

import Data.Thyme (UTCTime, getCurrentTime)
import System.Process (spawnCommand, waitForProcess)
import System.Exit (ExitCode(..))
import System.Clock (TimeSpec, getTime, Clock(Monotonic))
import Control.Monad

class (Applicative f) => MonotonicClock f a where
  getMonotonicTime :: f a

instance MonotonicClock IO TimeSpec where
  getMonotonicTime = getTime Monotonic

class WallClock f a where
  getWallTime :: f a

instance WallClock IO UTCTime where
  getWallTime = getCurrentTime

class Process m a where
  runSyncCommand :: a -> m ExitCode

instance Process IO String where
  runSyncCommand program = spawnCommand program >>= waitForProcess

class Console m a where
  printLine :: a -> m ()

instance Console IO String where
  printLine = putStrLn

data Measurement a b c d = StartTime a | Running c | EndTime a | TimeTaken b b | Completed ExitCode

type MonotonicMeasurement = Measurement UTCTime TimeSpec String String