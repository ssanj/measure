{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

data Measurement a b c = StartTime a | Running c | EndTime a | TimeTaken b b | Completed ExitCode

type MonotonicMeasurement = Measurement UTCTime TimeSpec String

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
    cmd:params    -> runMeasure (cmd:params)
    []            -> showBanner >> showCombinedVersion >> showCombinedHelp


class (Applicative f) => MonotonicClock f a where
  getMonotonicTime :: f a

instance MonotonicClock IO TimeSpec where
  getMonotonicTime = getTime Monotonic

-- How do we make this polymorphic?
runMeasure :: [String] -> IO ()
runMeasure args = do
  startPoint <- getMonotonicTime
  start      <- getCurrentTime
  let startTime = prettyMeasurement (StartTime start :: MonotonicMeasurement)
      program   = intercalate " " args
  putStrLn startTime -- add start time to the top in case the command never completes
  runProgram program
  end      <- getCurrentTime
  endPoint <- getMonotonicTime
  putStrLn startTime -- duplicate start time at the bottom where it's easier to see 
  putStrLn $ prettyMeasurement (EndTime end :: MonotonicMeasurement)
  putStrLn $ prettyMeasurement (TimeTaken startPoint endPoint :: MonotonicMeasurement)

runProgram :: String -> IO ()
runProgram program = do 
  putStrLn $ prettyMeasurement (Running program :: MonotonicMeasurement)
  handle   <- spawnCommand program
  exitCode <- waitForProcess handle
  putStrLn $ prettyMeasurement (Completed exitCode :: MonotonicMeasurement)


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
