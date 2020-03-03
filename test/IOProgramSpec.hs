{-# LANGUAGE ScopedTypeVariables #-}

module IOProgramSpec (test_all) where

import Test.Tasty                      (TestTree)
import Test.Tasty                      (testGroup)
import Test.Tasty.HUnit                (testCase)
import Test.Tasty.HUnit                ((@?=))

import IOProgram (mainProgam)
import CommandLine (CommandLineProcessor (..))

import Control.Monad.State.Lazy
import Data.Functor.Identity

test_all :: TestTree
test_all = 
  testGroup "IOProgram" 
    [
        withoutParams
      , withLongVersion
      , withShortVersion
      , withLongHelp
      , withShortHelp
      , withUtcTime
      , withLocalTime
      , withProgram
      , withUnknownShortParams
      , withUnknownLongParams
    ]

data ProcType = Set | NotSet deriving (Eq, Show)

type TestState = State ProcType

defaultProc :: CommandLineProcessor TestState
defaultProc = 
  CommandLineProcessor {
      runVersion       = put NotSet
    , runHelp          = put NotSet
    , runInfo          = put NotSet
    , runWithLocalTime = \_ -> put NotSet
    , runWithUtcTime   = \_ -> put NotSet
  }  

withoutParams :: TestTree
withoutParams = 
  let resultState = mainProgam [] (defaultProc { runInfo = put Set })
      cmdProc     = execState resultState NotSet
  in testCase "withoutParams" $ Set @?= cmdProc

withLongVersion :: TestTree
withLongVersion = 
  let resultState = mainProgam ["--version"] (defaultProc { runVersion = put Set })
      cmdProc     = execState resultState NotSet
  in testCase "withLongVersion" $ Set @?= cmdProc

withShortVersion :: TestTree
withShortVersion = 
  let resultState = mainProgam ["-v"] (defaultProc { runVersion = put Set })
      cmdProc     = execState resultState NotSet
  in testCase "withShortVersion" $ Set @?= cmdProc

withLongHelp :: TestTree
withLongHelp = 
  let resultState = mainProgam ["--help"] (defaultProc { runHelp = put Set })
      cmdProc     = execState resultState NotSet
  in testCase "withLongHelp" $ Set @?= cmdProc

withShortHelp :: TestTree
withShortHelp = 
  let resultState = mainProgam ["-h"] (defaultProc { runHelp = put Set })
      cmdProc     = execState resultState NotSet
  in testCase "withShortHelp" $ Set @?= cmdProc

withUtcTime :: TestTree
withUtcTime =
  let resultState = mainProgam ["--utc", "programx"] (defaultProc { runWithUtcTime = \_ -> put Set })
      cmdProc     = execState resultState NotSet
  in testCase "withUtcTime" $ Set @?= cmdProc

withLocalTime :: TestTree
withLocalTime =
  let resultState = mainProgam ["--local", "programx"] (defaultProc { runWithLocalTime = \_ -> put Set })
      cmdProc     = execState resultState NotSet
  in testCase "withLocalTime" $ Set @?= cmdProc

withProgram :: TestTree
withProgram =
  let resultState = mainProgam ["programx"] (defaultProc { runWithLocalTime = \_ -> put Set })
      cmdProc     = execState resultState NotSet
  in testCase "withProgram" $ Set @?= cmdProc

withUnknownParams :: [String] -> String -> TestTree
withUnknownParams args testCaseName =
  let resultState = mainProgam args (defaultProc { runInfo = put Set })
      cmdProc     = execState resultState NotSet
  in testCase testCaseName $ Set @?= cmdProc

withUnknownShortParams :: TestTree
withUnknownShortParams = withUnknownParams ["-blah"] "withUnknownShortParams"

withUnknownLongParams :: TestTree
withUnknownLongParams = withUnknownParams ["--blee"] "withUnknownLongParams"