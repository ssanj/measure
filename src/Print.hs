{-# LANGUAGE ScopedTypeVariables   #-}

module Print (prettyMeasurement, prettyMonotonicMeasure, prettyMonotonicLocalMeasure) where

import System.Exit (ExitCode(..))
import Format (green, yellow, red)
import Data.Text.Lazy (unpack)
import System.Clock (TimeSpec)
import Formatting.Clock (timeSpecs)
import Formatting (format)
import Data.Thyme (UTCTime, LocalTime)

import qualified Model as M

prettyMeasurement :: forall a b c d. M.Printable a b c d -> M.Measurement a b c d -> d
prettyMeasurement printField (M.StartTime value)         = M.printStartTime printField value
prettyMeasurement printField (M.Running value)           = M.printCommand printField value
prettyMeasurement printField (M.EndTime value)           = M.printEndTime printField value
prettyMeasurement printField (M.TimeTaken value1 value2) = M.printDuration printField value1 value2
prettyMeasurement printField (M.Completed value)         = M.printExitCode printField value

prettyMonotonicMeasure :: M.Printable UTCTime TimeSpec String String
prettyMonotonicMeasure = 
  M.Printable {
      M.printStartTime = measureoutValue "start"
    , M.printEndTime   = measureoutValue "end"
    , M.printDuration  = measureoutDuration "duration"
    , M.printCommand   = measureoutValue "running"
    , M.printExitCode  = measureoutExitCode "success" "failed"
  }

prettyMonotonicLocalMeasure :: M.Printable LocalTime TimeSpec String String
prettyMonotonicLocalMeasure = 
  M.Printable {
      M.printStartTime = measureoutValue "start"
    , M.printEndTime   = measureoutValue "end"
    , M.printDuration  = measureoutDuration "duration"
    , M.printCommand   = measureoutValue "running"
    , M.printExitCode  = measureoutExitCode "success" "failed"
  }

measureoutExitCode :: String -> String -> ExitCode -> String
measureoutExitCode success _ ExitSuccess        = measureoutPrefix success
measureoutExitCode _ failure (ExitFailure code) = measureoutError failure code

measureoutValue :: forall a. Show a => String -> a -> String
measureoutValue prefix value = (measureoutPrefix prefix) <> "[" <> (show value) <> "]"

measureoutDuration :: String -> TimeSpec -> TimeSpec -> String
measureoutDuration prefix value1 value2 = (measureoutPrefix prefix) <> "[" <> (unpack $ format (timeSpecs) value1 value2) <> "]"

measureoutError :: forall a. Show a => String -> a -> String
measureoutError prefix value = (green "measure:") <> (red prefix) <> "(exit code: " <> (show value) <> ")"

measureoutPrefix :: String -> String
measureoutPrefix prefix = (green "measure:") <> (yellow prefix)