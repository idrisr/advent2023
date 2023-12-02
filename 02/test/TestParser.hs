{-# LANGUAGE OverloadedStrings #-}

module TestParser (parseTests) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Game
import Parser
import Test.Tasty
import Test.Tasty.HUnit

parseTests :: TestTree
parseTests = testGroup "" [testParser, testRound, testMakeRound, testRounds]

game1BS :: B.ByteString
game1BS = "Game 1: 4 red, 3 blue; 1 red, 2 green, 6 blue; 2 green"

game1G :: Game
game1G =
    Game
        { gameID = 1
        , rounds =
            [ Round{red = 4, blue = 3, green = 0}
            , Round{red = 1, blue = 6, green = 2}
            , Round{red = 0, blue = 0, green = 2}
            ]
        }

game2BS :: B.ByteString
game2BS = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
game2G :: Game
game2G =
    Game
        { gameID = 2
        , rounds =
            [ Round{red = 0, blue = 1, green = 2}
            , Round{red = 1, blue = 4, green = 3}
            , Round{red = 0, blue = 1, green = 1}
            ]
        }

game3BS :: B.ByteString
game3BS = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
game3G :: Game
game3G =
    Game
        { gameID = 3
        , rounds =
            [ Round{red = 20, blue = 6, green = 8}
            , Round{red = 4, blue = 5, green = 13}
            , Round{red = 1, blue = 0, green = 5}
            ]
        }

game4BS :: B.ByteString
game4BS = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
game4G :: Game
game4G =
    Game
        { gameID = 4
        , rounds =
            [ Round{red = 3, blue = 6, green = 1}
            , Round{red = 6, blue = 0, green = 3}
            , Round{red = 14, blue = 15, green = 3}
            ]
        }

game5BS :: B.ByteString
game5BS = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
game5G :: Game
game5G =
    Game
        { gameID = 5
        , rounds =
            [ Round{red = 6, blue = 1, green = 3}
            , Round{red = 1, blue = 2, green = 2}
            ]
        }

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
           in [ testCase "1" $ f "2 green, 1 blue" @?= Right (Round{red = 0, blue = 1, green = 2})
              , testCase "2" $ f "2 green, 1 blue" @?= Right (Round{red = 0, blue = 1, green = 2})
              , testCase "3" $ f "2 green, 1 blue, 69 red" @?= Right (Round{red = 69, blue = 1, green = 2})
              ]

testRounds :: TestTree
testRounds =
    testGroup
        "rounds"
        $ let f = parseOnly getRounds
           in [ testCase "1" $
                    let sut = "2 green, 1 blue; 2 green, 1 blue"
                        got = f sut
                        wot = [Round{red = 0, blue = 1, green = 2}, Round{red = 0, blue = 1, green = 2}]
                     in got @?= Right wot
              ]

testMakeRound :: TestTree
testMakeRound =
    testGroup
        "make round"
        [ let
            got = makeRound emptyRound xs
            wot = Round{red = 1, blue = 2, green = 0}
            xs = [Red 1, Blue 2]
           in
            testCase "1" $ got @?= wot
        , let
            got = makeRound emptyRound xs
            wot = Round{red = 1, blue = 2, green = 8}
            xs = [Red 1, Blue 2, Green 8]
           in
            testCase "2" $ got @?= wot
        ]
