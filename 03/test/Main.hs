module Main (main) where

import Test.Tasty
import TestSchematic
import TestGear

tests :: TestTree
tests =
    testGroup
        "all"
        [ schematicTests
        , gearTests
        ]

main :: IO ()
main = defaultMain tests
