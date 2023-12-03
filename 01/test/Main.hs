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
        [ testCase "1" $ part1 "1abc2" @?= 12
        , testCase "2" $ part1 "pqr3stu8vwx" @?= 38
        , testCase "3" $ part1 "a1b2c3d4e5f" @?= 15
        , testCase "4" $ part1 "treb7uchet" @?= 77
        , testCase "5" $ part1 "two1three" @?= 11
        ]

tests2 :: TestTree
tests2 =
    testGroup
        "part 2"
        [ testCase "" $ part2 "two1nine" @?= 29
        , testCase "" $ part2 "eightwothree" @?= 83
        , testCase "" $ part2 "abcone2threexyz" @?= 13
        , testCase "" $ part2 "xtwone3four" @?= 24
        , testCase "" $ part2 "4nineeightseven2" @?= 42
        , testCase "" $ part2 "zoneight234" @?= 14
        , testCase "" $ part2 "7pqrstsixteen" @?= 76
        , testCase "" $ part2 "oneone" @?= 11
        , testCase "" $ part2 "one" @?= 11
        , testCase "" $ part2 "sz5" @?= 55
        , testCase "" $ part2 "0" @?= 0
        , testCase "" $ part2 "oneight" @?= 18
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
