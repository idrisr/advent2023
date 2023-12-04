module Main (main) where

import Test.Tasty
import TestCard

tests :: TestTree
tests = testGroup "all" [cardTests]

main :: IO ()
main = defaultMain tests
