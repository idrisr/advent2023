module TestGame where

import Test.Tasty
import Test.Tasty.HUnit
import Game
import TestData

gameTests :: TestTree
gameTests = testGroup "Game" [testPower]

-- 48. In games 2-5 it was 12, 1560, 630, and 36

testPower :: TestTree
testPower =
    testGroup
        "Power"
        $ let f g = power $ minContents g
           in [ testCase "1" $ f game1G @?= 48
              , testCase "2" $ f game2G @?= 12
              , testCase "3" $ f game3G @?= 1560
              , testCase "4" $ f game4G @?= 630
              , testCase "5" $ f game5G @?= 36
              ]
