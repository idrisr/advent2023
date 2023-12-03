{-# LANGUAGE OverloadedStrings #-}

module TestParser (parseTests) where

import Prelude hiding (round)
import Data.Attoparsec.ByteString.Char8
import Game
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import TestData
import Lens.Micro.Platform

parseTests :: TestTree
parseTests = testGroup "" [testParser, testRound, testMakeRound, testRounds]

testParser :: TestTree
testParser =
    testGroup
        "parser"
        $ let f = parseOnly getGame
           in [ testCase "Game 1" $ f game1BS @?= Right game1G
              , testCase "Game 2" $ f game2BS @?= Right game2G
              , testCase "Game 3" $ f game3BS @?= Right game3G
              , testCase "Game 4" $ f game4BS @?= Right game4G
              , testCase "Game 5" $ f game5BS @?= Right game5G
              ]

testRound :: TestTree
testRound =
    testGroup
        "round"
        $ let f = parseOnly getRound
           in [ let wot = round & green .~ 2 & blue .~ 1
                    got = f "2 green, 1 blue"
                 in testCase "1" $ got @?= Right wot
              , let wot = round & green .~ 1 & blue .~ 2
                    got = f "1 green, 2 blue"
                 in testCase "2" $ got @?= Right wot
              , let wot = round & red .~ 69 & green .~ 2 & blue .~ 1
                    got = f "2 green, 1 blue, 69 red"
                 in testCase "2" $ got @?= Right wot
              ]

testRounds :: TestTree
testRounds =
    testGroup
        "rounds"
        $ let f = parseOnly getRounds
           in [ testCase "1" $
                    let sut = "2 green, 1 blue; 2 green, 5150 blue"
                        got = f sut
                        wot =
                            [ round & green .~ 2 & blue .~ 1
                            , round & green .~ 2 & blue .~ 5150
                            ]
                     in got @?= Right wot
              ]

testMakeRound :: TestTree
testMakeRound =
    testGroup
        "make round"
        [ let
            got = makeRound round xs
            wot = round & red .~ 1 & blue .~ 2
            xs = [Red 1, Blue 2]
           in
            testCase "1" $ got @?= wot
        , let
            got = makeRound round xs
            wot = round & red .~ 1 & green .~ 8 & blue .~ 2
            xs = [Red 1, Blue 2, Green 8]
           in
            testCase "2" $ got @?= wot
        ]
