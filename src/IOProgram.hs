module IOProgram (mainProgam, mainProgramWithDefaults) where

import Paths_measure (version)
import Data.List (intercalate, isPrefixOf)
import System.Environment (getArgs)

import qualified Data.Version as DV

import Data.List.NonEmpty (NonEmpty(..))
import Print (prettyMeasurement, prettyMonotonicMeasure, prettyMonotonicLocalMeasure)
import Runner (runProgram)
import Measure (runMeasure)
import CommandLine (CommandLineProcessor(..))

showHelp, showVersion, showBanner, showCombinedVersion, showCombinedHelp :: IO ()

showHelp = putStrLn "usage: measure <command>"

showCombinedHelp = putStrLn " usage: measure <command>"

showVersion = putStrLn $ "measure version " <> (DV.showVersion version)

showCombinedVersion = putStrLn $ " version " <> (DV.showVersion version)

showBanner = putStrLn banner

-- TODO: Can we reuse a single function with multiple pretty functions?
withLocalTime :: NonEmpty String -> IO ()
withLocalTime(p1 :| other) = runMeasure (intercalate " " (p1:other)) (runProgram $ prettyMeasurement prettyMonotonicLocalMeasure) (prettyMeasurement prettyMonotonicLocalMeasure)

withUtcTime :: NonEmpty String -> IO ()
withUtcTime (p1 :| other) = runMeasure (intercalate " " (p1:other)) (runProgram $ prettyMeasurement prettyMonotonicMeasure) (prettyMeasurement prettyMonotonicMeasure)

banner :: String
banner = "\
\  _ __ ___   ___  __ _ ___ _   _ _ __ ___ \n\
\ | '_ ` _ \\ / _ \\/ _` / __| | | | '__/ _ \n\
\ | | | | | |  __/ (_| \\__ \\ |_| | | |  __/\n\
\ |_| |_| |_|\\___|\\__,_|___/\\__,_|_|  \\___|\n\
\                                          \n\
\"

commandLineProc :: CommandLineProcessor IO
commandLineProc = 
  CommandLineProcessor {
      runVersion       = showVersion
    , runHelp          = showHelp
    , runInfo          = showBanner >> showCombinedVersion >> showCombinedHelp
    , runWithLocalTime = withLocalTime
    , runWithUtcTime   = withUtcTime
  }  

mainProgam :: [String] -> CommandLineProcessor f -> f ()
mainProgam args proc =
  case args of
    (a1: ax) | isPrefixOf "-" a1 ->
      case (a1 : ax) of
        ["--version"]         -> runVersion proc
        ["-v"]                -> runVersion proc
        ["--help"]            -> runHelp proc
        ["-h"]                -> runHelp proc
        ("--utc"   : p1 : px) -> runWithUtcTime   proc (p1 :| px)
        ("--local" : p1 : px) -> runWithLocalTime proc (p1 :| px)
        _                 -> runInfo proc
    (p1 : px)                 -> runWithLocalTime proc (p1 :| px)
    _                     -> runInfo proc


mainProgramWithDefaults :: IO ()
mainProgramWithDefaults = getArgs >>= (flip mainProgam) commandLineProc

