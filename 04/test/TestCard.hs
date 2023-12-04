{-# LANGUAGE OverloadedStrings #-}

module TestCard (cardTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Attoparsec.ByteString.Char8
import Control.Lens
import Types
import Parser
import TestData

cardTests :: TestTree
cardTests =
    testGroup
        "Card"
        [ valueTests
        , matchTests
        , parserTests
        , updateTests
        ]

valueTests :: TestTree
valueTests = testGroup "cardValue" [
    testCase "1" $ c1^.pointValue @?= 8
    , testCase "2" $ c2^.pointValue  @?= 2
    , testCase "3" $ c3^.pointValue  @?= 2
    , testCase "4" $ c4^.pointValue  @?= 1
    , testCase "5" $ c5^.pointValue  @?= 0
    , testCase "6" $ c6^.pointValue  @?= 0
    ]

matchTests :: TestTree
matchTests = testGroup "matches" [
    testCase "1" $ c1^.matches @?= 4
    , testCase "2" $ c2^.matches  @?= 2
    , testCase "3" $ c3^.matches  @?= 2
    , testCase "4" $ c4^.matches  @?= 1
    , testCase "5" $ c5^.matches  @?= 0
    , testCase "6" $ c6^.matches  @?= 0
    ]

parserTests :: TestTree
parserTests =
    testGroup "parser" $
        let f = parseOnly getCard
         in [ let got = f s1
                  wot = Right c1
               in testCase "1" $ got @?= wot
            , let got = f s2
                  wot = Right c2
               in testCase "2" $ got @?= wot
            , let got = f s3
                  wot = Right c3
               in testCase "3" $ got @?= wot
            ]

updateTests :: TestTree
updateTests =
    testGroup "updates" $
        let sut = updateCards cards
         in [ let got = head sut^.copies
                  wot = 1
               in testCase "1" $ got @?= wot
            , let got = (sut !! 1)^.copies
                  wot = 2
               in testCase "2" $ got @?= wot
            , let got = (sut !! 2)^.copies
                  wot = 4
               in testCase "3" $ got @?= wot
            , let got = (sut !! 3)^.copies
                  wot = 8
               in testCase "4" $ got @?= wot
            , let got = (sut !! 4)^.copies
                  wot = 14
               in testCase "5" $ got @?= wot
            , let got = (sut !! 5)^.copies
                  wot = 1
               in testCase "6" $ got @?= wot
            ]
