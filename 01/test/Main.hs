{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Day01
import Parser
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "" [tests1, tests2, testParse]

tests1 :: TestTree
tests1 =
    testGroup
        "part 1"
        [ testCase "1" $ firstLast "1abc2" @?= 12
        , testCase "2" $ firstLast "pqr3stu8vwx" @?= 38
        , testCase "3" $ firstLast "a1b2c3d4e5f" @?= 15
        , testCase "4" $ firstLast "treb7uchet" @?= 77
        ]

tests2 :: TestTree
tests2 =
    testGroup
        "part 2"
        [ testCase "" $ firstLast "two1nine" @?= 29
        , testCase "" $ firstLast "eightwothree" @?= 83
        , testCase "" $ firstLast "abcone2threexyz" @?= 13
        , testCase "" $ firstLast "xtwone3four" @?= 24
        , testCase "" $ firstLast "4nineeightseven2" @?= 42
        , testCase "" $ firstLast "zoneight234" @?= 14
        , testCase "" $ firstLast "7pqrstsixteen" @?= 76
        , testCase "" $ firstLast "oneone" @?= 11
        , testCase "" $ firstLast "one" @?= 11
        , testCase "" $ firstLast "sz5" @?= 55
        , testCase "" $ firstLast "0" @?= 0
        , testCase "" $ firstLast "oneight" @?= 18
        ]

testParse :: TestTree
testParse =
    testGroup
        "parser"
        $ let f = getNumbers parseFLine
           in [ testCase "" $ f "two1nine" @?= [2, 1, 9]
              , testCase "" $ f "eightwothree" @?= [8, 3]
              , testCase "" $ f "abcone2threexyz" @?= [1, 2, 3]
              , testCase "" $ f "xtwone3four" @?= [2, 3, 4]
              , testCase "" $ f "zoneight234" @?= [1, 2, 3, 4]
              , testCase "" $ f "1" @?= [1]
              , testCase "" $ f "one" @?= [1]
              , testCase "" $ f "1one" @?= [1, 1]
              , testCase "" $ f "one1one" @?= [1, 1, 1]
              , testCase "" $ f "twoone1one" @?= [2, 1, 1, 1]
              , testCase "" $ f "atwoone1one" @?= [2, 1, 1, 1]
              , testCase "" $ f "" @?= []
              ]

main :: IO ()
main = defaultMain tests
