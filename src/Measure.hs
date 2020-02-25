{-# LANGUAGE ScopedTypeVariables   #-}

module Measure (runMeasure) where

import qualified Model as M

runMeasure :: forall f a b c d. (Monad f, M.WallClock f a, M.MonotonicClock f b, M.Console f d) => c -> (c -> f()) -> (M.Measurement a b c d -> d) -> f ()
runMeasure program programRunner pretty = do
  startPoint <- M.getMonotonicTime
  start      <- M.getWallTime
  let startTime = pretty (M.StartTime start)
  M.printLine startTime -- add start time to the top in case the command never completes
  programRunner program
  end      <- M.getWallTime
  endPoint <- M.getMonotonicTime
  M.printLine startTime -- duplicate start time at the bottom where it's easier to see 
  M.printLine $ pretty (M.EndTime end)
  M.printLine $ pretty (M.TimeTaken startPoint endPoint)
