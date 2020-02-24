module IOProgram (mainProgam) where

import Paths_measure (version)
import Data.List (intercalate)
import System.Environment (getArgs)

import qualified Data.Version as DV

import Print (prettyMeasurement)
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

mainProgam = do
  args <- getArgs
  case args of
    ["--version"] -> showVersion
    ["-v"]        -> showVersion
    ["--help"]    -> showHelp
    ["-h"]        -> showHelp
    params@(_:_)    -> runMeasure (intercalate " " params) (runProgram prettyMeasurement) prettyMeasurement
    []            -> showBanner >> showCombinedVersion >> showCombinedHelp
