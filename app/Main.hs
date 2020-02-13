{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Thyme

import Paths_measure (version)
import Data.AffineSpace ((.-.), Diff)
import Data.List (intercalate)
import System.Process (spawnCommand, waitForProcess)
import System.Exit (ExitCode(..))
import System.Environment (getArgs)

import qualified Data.Version as DV

data Measurement a b c = StartTime a | Running c | EndTime a | TimeTaken b | Completed ExitCode

type UTCMeasurement = Measurement UTCTime (Diff UTCTime) String

showHelp, showVersion, main :: IO ()

showHelp = putStrLn "usage: measure <command>"

showVersion = putStrLn $ "measure version " <> (DV.showVersion version)

main = do
  args <- getArgs
  case args of
    ["--version"] -> showVersion
    ["--help"]    -> showHelp
    cmd:params    -> runMeasure (cmd:params)
    []            -> showHelp

runMeasure :: [String] -> IO ()
runMeasure args = do
  start <- getCurrentTime
  putStrLn $ prettyMeasurement (StartTime start :: UTCMeasurement)
  let program = intercalate " " args
  runProgram program
  end <- getCurrentTime
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
prettyMeasurement (StartTime value) = measureout "start"   value
prettyMeasurement (Running value)   = measureout "running" value
prettyMeasurement (EndTime value)   = measureout "end"     value
prettyMeasurement (TimeTaken value) = measureout "time"    value
prettyMeasurement (Completed ExitSuccess)        = measureout "success"   (0 :: Int)
prettyMeasurement (Completed (ExitFailure code)) = measureout "failed"    code

measureout :: forall a. Show a => String -> a -> String
measureout prefix value = "measure:" <> prefix <> "[" <> (show value) <> "]"
