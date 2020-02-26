{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Model(
  -- * Types  
    MonotonicClock
  , WallClock 
  , Process 
  , Console 
  , Measurement(..) 
  , MonotonicMeasurement
  , MonotonicLocalMeasurement
  , Printable(..)
  -- * Functions
  , getMonotonicTime
  , getWallTime
  , runSyncCommand
  , printLine
) where

import Data.Thyme (UTCTime, LocalTime, getCurrentTime, getCurrentTimeZone)
import Data.Thyme.Time.Core (utcToLocalTime)
import System.Process (spawnCommand, waitForProcess)
import System.Exit (ExitCode(..))
import System.Clock (TimeSpec, getTime, Clock(Monotonic))

class MonotonicClock f a where
  getMonotonicTime :: f a

instance MonotonicClock IO TimeSpec where
  getMonotonicTime = getTime Monotonic

class WallClock f a where
  getWallTime :: f a

instance WallClock IO UTCTime where
  getWallTime = getCurrentTime

instance WallClock IO LocalTime where
  getWallTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

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

type MonotonicLocalMeasurement = Measurement LocalTime TimeSpec String String

data Printable a b c d = 
  Printable {  
      printStartTime :: a -> d
    , printEndTime   :: a -> d
    , printDuration  :: b -> b -> d
    , printCommand   :: c -> d
    , printExitCode  :: ExitCode -> d
  }