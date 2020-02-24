{-# LANGUAGE ScopedTypeVariables   #-}

module Print where

import System.Exit (ExitCode(..))
import Format (green, yellow, red)
import Data.Text.Lazy (unpack)
import System.Clock (TimeSpec)
import Formatting.Clock (timeSpecs)
import Formatting (format)

import qualified Model as M

prettyMeasurement :: M.MonotonicMeasurement -> String
prettyMeasurement (M.StartTime value)              = measureoutValue    "start"    value
prettyMeasurement (M.Running value)                = measureoutValue    "running"  value
prettyMeasurement (M.EndTime value)                = measureoutValue    "end"      value
prettyMeasurement (M.TimeTaken value1 value2)      = measureoutDuration "duration" value1 value2
prettyMeasurement (M.Completed ExitSuccess)        = measureoutPrefix   "success"
prettyMeasurement (M.Completed (ExitFailure code)) = measureoutError    "failed"   code

measureoutValue :: forall a. Show a => String -> a -> String
measureoutValue prefix value = (measureoutPrefix prefix) <> "[" <> (show value) <> "]"

measureoutDuration :: String -> TimeSpec -> TimeSpec -> String
measureoutDuration prefix value1 value2 = (measureoutPrefix prefix) <> "[" <> (unpack $ format (timeSpecs) value1 value2) <> "]"

measureoutError :: forall a. Show a => String -> a -> String
measureoutError prefix value = (green "measure:") <> (red prefix) <> "(exit code: " <> (show value) <> ")"

measureoutPrefix :: String -> String
measureoutPrefix prefix = (green "measure:") <> (yellow prefix)