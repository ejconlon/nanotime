module Main
  ( main
  )
where

import Data.Word (Word64)
import Nanotime
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

nsPerSec :: Word64
nsPerSec = 1000000000

testDelta :: TestTree
testDelta = testCase "delta" $ do
  TimeDelta SignPos 0 == TimeDelta SignNeg 0 @?= True
  TimeDelta SignPos 1 == TimeDelta SignPos 1 @?= True
  TimeDelta SignPos 1 == TimeDelta SignNeg 1 @?= False
  TimeDelta SignPos 1 == TimeDelta SignPos 2 @?= False
  compare (TimeDelta SignPos 0) (TimeDelta SignNeg 0) @?= EQ
  compare (TimeDelta SignPos 1) (TimeDelta SignPos 1) @?= EQ
  compare (TimeDelta SignPos 1) (TimeDelta SignNeg 1) @?= GT
  compare (TimeDelta SignPos 1) (TimeDelta SignPos 2) @?= LT
  0 @?= TimeDelta SignPos 0
  mempty @TimeDelta @?= 0
  timeDeltaFromFracSecs @Rational 0 @?= 0
  timeDeltaFromFracSecs @Rational 1 @?= TimeDelta SignPos nsPerSec
  timeDeltaFromFracSecs @Rational (-1) @?= TimeDelta SignNeg nsPerSec
  negate @TimeDelta 0 @?= 0
  negate (TimeDelta SignPos 1) @?= TimeDelta SignNeg 1
  negate (TimeDelta SignNeg 1) @?= TimeDelta SignPos 1
  TimeDelta SignPos 1 + TimeDelta SignPos 2 @?= TimeDelta SignPos 3
  TimeDelta SignPos 1 - TimeDelta SignPos 2 @?= TimeDelta SignNeg 1
  TimeDelta SignPos 2 - TimeDelta SignPos 1 @?= TimeDelta SignPos 1

main :: IO ()
main =
  defaultMain $
    testGroup
      "Nanotime"
      [ testDelta
      ]
