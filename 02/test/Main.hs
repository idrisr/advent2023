module Main (main) where

import Test.Tasty
import TestParser

tests :: TestTree
tests = testGroup "all" [parseTests]

main :: IO ()
main = defaultMain tests
