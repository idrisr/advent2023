{-# LANGUAGE OverloadedStrings #-}

module TestTypes (sortTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Types
import Control.Lens

sortTests :: TestTree
sortTests =
    testGroup
        ""
        [ testSort
        , testHands
        , testHands2
        , testSortHand
        ]

hand6, hand4, hand5, hand3, hand2, hand1 :: Hand
hand1 = Hand (Label "QQQJA") 0
hand2 = Hand (Label "T55J5") 0
hand3 = Hand (Label "32T3K") 0
hand4 = Hand (Label "AQ8KT") 0
hand5 = Hand (Label "44446") 0
hand6 = Hand (Label "23JTA") 0

testSort :: TestTree
testSort =
    testGroup
        "sort"
        [ testCase "" $ assertBool "" (hand1 > hand2)
        , testCase "" $ assertBool "" (hand3 < hand1)
        , testCase "" $ assertBool "" (hand5 > hand4)
        ]

testSortHand :: TestTree
testSortHand =
    testGroup
        "type sort"
        [ testCase "" $ assertBool "" (ThreeOfKind > OnePair)
        , testCase "" $ assertBool "" (FourOfKind > OnePair)
        , testCase "" $ assertBool "" (FiveOfKind > OnePair)
        , testCase "" $ assertBool "" (FiveOfKind > OnePair)
        , testCase "" $ assertBool "" (TwoPair > OnePair)
        , testCase "" $ assertBool "" (FourOfKind > HighCard)
        ]

testHands :: TestTree
testHands =
    testGroup
        "hands"
        [ testCase "" $ handType (hand1 ^. label) @?= ThreeOfKind
        , testCase "" $ handType (hand2 ^. label) @?= ThreeOfKind
        , testCase "" $ handType (hand3 ^. label) @?= OnePair
        , testCase "" $ handType (hand4 ^. label) @?= HighCard
        , testCase "" $ handType (hand5 ^. label) @?= FourOfKind
        ]

testHands2 :: TestTree
testHands2 =
    testGroup
        "hands"
        [ testCase "" $ handType2 (hand6 ^. label) @?= OnePair
        ]
