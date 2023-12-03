module TestSchematic (schematicTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Schematic
import Control.Lens
import Types
import TestData

schematicTests :: TestTree
schematicTests =
    testGroup
        "schematic"
        [ testMakeLine
        , testLineUpdate
        , testSchematicTotal
        ]

testMakeLine :: TestTree
testMakeLine = testGroup "make line" [
    testCase "1" $ makeLine s1 @?= line1
  , testCase "2" $ makeLine s2 @?= line2
  , testCase "3" $ makeLine s3 @?= line3
  , testCase "4" $ makeLine s4 @?= line4
  , testCase "5" $ makeLine "" @?= line
    ]

testLines :: [Line]
testLines =
        makeLine
        <$>
        [ "...*......"
        , "..35..633."
        , "......#..."
        ]

testLineUpdate :: TestTree
testLineUpdate =
    let
        line0, line1a, line2a :: Line
        line0 = head testLines
        line1a = testLines !! 1
        line2a = testLines !! 2
     in
        testGroup
            "lineUpdate"
            [ let sut = line0
                  got = updateLine Nothing sut (Just line1)
               in testCase "" $ sut @?= got
            , let sut = line1a
                  got = updateLine (Just line0) sut (Just line2a)
                  wot =
                    line1a
                        -- almost certainly a better way to do this with opics
                        & numbers
                            .~ [ number & startIndex .~ 2 & endIndex .~ 3 & numValue .~ 35 & symbolAdjacent .~ True
                               , number & startIndex .~ 6 & endIndex .~ 8 & numValue .~ 633 & symbolAdjacent .~ True
                               ]
               in testCase "" $ wot @?= got
            , let sut = line2a
                  got = updateLine (Just line1) sut Nothing
               in testCase "" $ sut @?= got
            ]

testSchematicTotal :: TestTree
testSchematicTotal =
    testGroup
        "schematic total"
        [ let ls = updateLines testLines1
              got = totalSchematic (Schematic ls)
              wot = 4361
           in testCase "" $ got @?= wot
        ]
