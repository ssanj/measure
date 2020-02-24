{-# LANGUAGE ScopedTypeVariables   #-}

module Runner (runProgram) where

import qualified Model as M

runProgram :: forall f a b c d. (Monad f, M.Process f c, M.Console f d) => (M.Measurement a b c d -> d) -> c -> f ()
runProgram pretty program = do 
  M.printLine $ pretty (M.Running program)
  exitCode <- M.runSyncCommand program
  M.printLine $ pretty (M.Completed exitCode)
