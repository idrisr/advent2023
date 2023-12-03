module Main (main) where

import Test.Tasty
import TestParser
import TestGame

tests :: TestTree
tests = testGroup "all" [parseTests, gameTests]

main :: IO ()
main = defaultMain tests
