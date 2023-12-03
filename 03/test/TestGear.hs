module TestGear (gearTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Schematic
import Types
import TestData

gearTests :: TestTree
gearTests = testGroup "gears" [testGearTotal]

testGearTotal :: TestTree
testGearTotal =
    testGroup
        "gear total"
        [ let ls = updateLines testLines1
              got = totalGears (Schematic ls)
              wot = 467835
           in testCase "" $ got @?= wot
        ]
