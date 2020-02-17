{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Thyme

import Paths_measure (version)
import Data.AffineSpace ((.-.), Diff)
import Data.List (intercalate)
import System.Process (spawnCommand, waitForProcess)
import System.Exit (ExitCode(..))
import System.Environment (getArgs)
import Format (green, yellow, red)

import qualified Data.Version as DV

data Measurement a b c = StartTime a | Running c | EndTime a | TimeTaken b | Completed ExitCode

type UTCMeasurement = Measurement UTCTime (Diff UTCTime) String

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

runMeasure :: [String] -> IO ()
runMeasure args = do
  start <- getCurrentTime
  let startTime = prettyMeasurement (StartTime start :: UTCMeasurement)
      program   = intercalate " " args
  putStrLn startTime -- add start time to the top in case the command never completes
  runProgram program
  end <- getCurrentTime
  putStrLn startTime -- duplicate start time at the bottom where it's easier to see 
  putStrLn $ prettyMeasurement (EndTime end :: UTCMeasurement)
  let diff = end .-. start :: Diff UTCTime
  putStrLn $ prettyMeasurement (TimeTaken diff :: UTCMeasurement)

runProgram :: String -> IO ()
runProgram program = do 
  putStrLn $ prettyMeasurement (Running program :: UTCMeasurement)
  handle   <- spawnCommand program
  exitCode <- waitForProcess handle
  putStrLn $ prettyMeasurement (Completed exitCode :: UTCMeasurement)


prettyMeasurement :: forall a b c. (Show a, Show b, Show c) => Measurement a b c -> String
prettyMeasurement (StartTime value)              = measureoutValue  "start"   value
prettyMeasurement (Running value)                = measureoutValue  "running" value
prettyMeasurement (EndTime value)                = measureoutValue  "end"     value
prettyMeasurement (TimeTaken value)              = measureoutValue  "time"    value
prettyMeasurement (Completed ExitSuccess)        = measureoutPrefix "success"
prettyMeasurement (Completed (ExitFailure code)) = measureoutError  "failed"  code

measureoutValue :: forall a. Show a => String -> a -> String
measureoutValue prefix value = (measureoutPrefix prefix) <> "[" <> (show value) <> "]"

measureoutError :: forall a. Show a => String -> a -> String
measureoutError prefix value = (green "measure:") <> (red prefix) <> "(exit code: " <> (show value) <> ")"

measureoutPrefix :: String -> String
measureoutPrefix prefix = (green "measure:") <> (yellow prefix)
