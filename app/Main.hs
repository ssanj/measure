{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Thyme

import Data.AffineSpace ((.-.), Diff)
import Data.List (intercalate)
import System.Process (spawnCommand, waitForProcess)
import System.Exit (ExitCode(..))
import System.Environment (getArgs)

data Measurement a b c = StartTime a | Running c | EndTime a | TimeTaken b | Completed ExitCode

type UTCMeasurement = Measurement UTCTime (Diff UTCTime) String

main :: IO ()
main = do
  start <- getCurrentTime
  putStrLn $ prettyMeasurement (StartTime start :: UTCMeasurement)
  args <- getArgs
  let exec = intercalate " " args
  run exec
  end <- getCurrentTime
  putStrLn $ prettyMeasurement (EndTime end :: UTCMeasurement)
  let diff = end .-. start :: Diff UTCTime
  putStrLn $ prettyMeasurement (TimeTaken diff :: UTCMeasurement)

run :: String -> IO ()
run exec = do 
  putStrLn $ prettyMeasurement (Running exec :: UTCMeasurement)
  handle   <- spawnCommand exec
  exitCode <- waitForProcess handle
  putStrLn $ prettyMeasurement (Completed exitCode :: UTCMeasurement)


prettyMeasurement :: forall a b c. (Show a, Show b, Show c) => Measurement a b c -> String
prettyMeasurement (StartTime value) = measureout "start"   value
prettyMeasurement (Running value)   = measureout "running" value
prettyMeasurement (EndTime value)   = measureout "end"     value
prettyMeasurement (TimeTaken value) = measureout "time"    value
prettyMeasurement (Completed ExitSuccess)        = measureout "success"   0
prettyMeasurement (Completed (ExitFailure code)) = measureout "failed"    code

measureout :: forall a. Show a => String -> a -> String
measureout prefix value = "measure:" <> prefix <> "[" <> (show value) <> "]"