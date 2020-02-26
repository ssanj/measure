{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module MeasureSpec (test_all) where

import Test.Tasty                      (TestTree)
import Test.Tasty                      (testGroup)
import Test.Tasty.HUnit                (testCase)
import Test.Tasty.HUnit                ((@?=))
import Measure (runMeasure)

import qualified Model as M

import Control.Monad.State.Lazy
import Data.Functor.Identity

data TestData a b c d = TestData { wallClock :: a, montonicClock :: b, console :: [c], run :: Maybe d }

type StateTestData = State (TestData Int Int String String)

instance M.MonotonicClock StateTestData Int where
  getMonotonicTime =  do
    tdx  <- get
    modify (\td -> td {montonicClock = 2 * (montonicClock td) })
    pure $ montonicClock tdx

instance M.WallClock StateTestData Int where
  getWallTime =  do
    tdx  <- get
    modify (\td -> td {wallClock = 2 * (wallClock td) })
    pure $ wallClock tdx

instance M.Console StateTestData String where
  printLine toPrint = modify (\td -> td { console =  toPrint : console td })


programRunner :: String -> StateTestData ()
programRunner prog = modify (\td -> td { run =  Just prog })

program :: String
program = "some-program"

prettyPrint :: M.Measurement Int Int String String -> String
prettyPrint (M.StartTime start)               = "StartTime: " <> (show start)
prettyPrint (M.EndTime end)                   = "EndTime: " <> (show end)
prettyPrint (M.TimeTaken startPoint endPoint) = "TimeTaken: (" <> (show startPoint) <> "," <> (show endPoint) <> ")"
prettyPrint (M.Running prog)                  = "Running: " <> prog
prettyPrint (M.Completed code)                = "Completed: " <> (show code)

defaultState :: TestData Int Int String String
defaultState = TestData 20000 100 [] Nothing

test_all :: TestTree
test_all = testGroup "Measure" [withRunMeasure]

withRunMeasure :: TestTree
withRunMeasure = 
  let programState = runMeasure program programRunner prettyPrint
      result :: TestData Int Int String String = execState programState defaultState
  in  testCase "runMeasure" $ do
        ["StartTime: 20000", "StartTime: 20000", "EndTime: 40000", "TimeTaken: (100,200)"] @?= (reverse . console $ result)
        Just program @?= run result
