module Main (main) where

import Test.Tasty
import TestSchematic

tests :: TestTree
tests = testGroup "all" [schematicTests]

main :: IO ()
main = defaultMain tests
