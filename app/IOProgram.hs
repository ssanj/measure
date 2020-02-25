module IOProgram (mainProgam) where

import Paths_measure (version)
import Data.List (intercalate, isPrefixOf)
import System.Environment (getArgs)

import qualified Data.Version as DV

import Print (prettyMeasurement, prettyMonotonicMeasure, prettyMonotonicLocalMeasure)
import Runner (runProgram)
import Measure (runMeasure)

mainProgam, showHelp, showVersion, showBanner, showCombinedVersion, showCombinedHelp :: IO ()

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

-- TODO: Clean this up
-- match by prefix (--) and then validate all valid arguments
mainProgam = do
  args <- getArgs
  case args of
    x | x == ["--version"] || x == ["-v"] -> showVersion
    x | x == ["--help"]    || x == ["-h"] -> showHelp
    ("--utc":p1:other)   -> runMeasure (intercalate " " (p1:other)) (runProgram $ prettyMeasurement prettyMonotonicMeasure) (prettyMeasurement prettyMonotonicMeasure)
    ("--local":p1:other) -> runMeasure (intercalate " " (p1:other)) (runProgram $ prettyMeasurement prettyMonotonicLocalMeasure) (prettyMeasurement prettyMonotonicLocalMeasure)
    (p1:other) | not $ isPrefixOf "--" p1 -> runMeasure (intercalate " " (p1:other)) (runProgram $ prettyMeasurement prettyMonotonicLocalMeasure) (prettyMeasurement prettyMonotonicLocalMeasure)
    _ -> showBanner >> showCombinedVersion >> showCombinedHelp    