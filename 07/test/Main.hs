module Main (main) where

import Test.Tasty
import TestTypes

tests :: TestTree
tests = testGroup "all" [sortTests]

main :: IO ()
main = defaultMain tests
