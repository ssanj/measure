{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Main where

import Data.Thyme

import Paths_measure (version)
import Data.List (intercalate)
import Data.Text.Lazy (unpack)
import System.Process (spawnCommand, waitForProcess)
import System.Exit (ExitCode(..))
import System.Environment (getArgs)
import Format (green, yellow, red)

import System.Clock
import Formatting
import Formatting.Clock

import qualified Data.Version as DV

data Measurement a b c d = StartTime a | Running c | EndTime a | TimeTaken b b | Completed ExitCode

type MonotonicMeasurement = Measurement UTCTime TimeSpec String String

main, showHelp, showVersion, showBanner, showCombinedVersion, showCombinedHelp :: IO ()

showHelp = putStrLn "usage: measure <command>"

showCombinedHelp = putStrLn " usage: measure <command>"

showVersion = putStrLn $ "measure version " <> (DV.showVersion version)

showCombinedVersion = putStrLn $ " version " <> (DV.showVersion version)

showBanner = putStrLn banner

banner :: String
banner = "\
\  _ __ ___   ___  __ _ ___ _   _ _ __ ___ \n\
\ | '_ ` _ \\ / _ \\/ _` / __| | | | '__/ _ \n\
\ | | | | | |  __/ (_| \\__ \\ |_| | | |  __/\n\
\ |_| |_| |_|\\___|\\__,_|___/\\__,_|_|  \\___|\n\
\                                          \n\
\"

main = do
  args <- getArgs
  case args of
    ["--version"] -> showVersion
    ["-v"]        -> showVersion
    ["--help"]    -> showHelp
    ["-h"]        -> showHelp
    params@(_:_)    -> runMeasure (intercalate " " params) (runProgram prettyMeasurement) prettyMeasurement
    []            -> showBanner >> showCombinedVersion >> showCombinedHelp


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

runMeasure :: forall f a b c d. (Monad f, WallClock f a, MonotonicClock f b, Process f c, Console f d) => c -> (c -> f()) -> (Measurement a b c d -> d) -> f ()
runMeasure program programRunner pretty = do
  startPoint <- getMonotonicTime
  start      <- getWallTime
  let startTime = pretty (StartTime start)
  printLine startTime -- add start time to the top in case the command never completes
  programRunner program
  end      <- getWallTime
  endPoint <- getMonotonicTime
  printLine startTime -- duplicate start time at the bottom where it's easier to see 
  printLine $ pretty (EndTime end)
  printLine $ pretty (TimeTaken startPoint endPoint)

runProgram :: forall f a b c d. (Monad f, Process f c, Console f d) => (Measurement a b c d -> d) -> c -> f ()
runProgram pretty program = do 
  printLine $ pretty (Running program)
  exitCode <- runSyncCommand program
  printLine $ pretty (Completed exitCode)

-- TODO: Move this out into a separate module
prettyMeasurement :: MonotonicMeasurement -> String
prettyMeasurement (StartTime value)              = measureoutValue    "start"    value
prettyMeasurement (Running value)                = measureoutValue    "running"  value
prettyMeasurement (EndTime value)                = measureoutValue    "end"      value
prettyMeasurement (TimeTaken value1 value2)      = measureoutDuration "duration" value1 value2
prettyMeasurement (Completed ExitSuccess)        = measureoutPrefix   "success"
prettyMeasurement (Completed (ExitFailure code)) = measureoutError    "failed"   code

measureoutValue :: forall a. Show a => String -> a -> String
measureoutValue prefix value = (measureoutPrefix prefix) <> "[" <> (show value) <> "]"

measureoutDuration :: String -> TimeSpec -> TimeSpec -> String
measureoutDuration prefix value1 value2 = (measureoutPrefix prefix) <> "[" <> (unpack $ format (timeSpecs) value1 value2) <> "]"

measureoutError :: forall a. Show a => String -> a -> String
measureoutError prefix value = (green "measure:") <> (red prefix) <> "(exit code: " <> (show value) <> ")"

measureoutPrefix :: String -> String
measureoutPrefix prefix = (green "measure:") <> (yellow prefix)
