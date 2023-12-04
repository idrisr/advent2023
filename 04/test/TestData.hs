{-# LANGUAGE OverloadedStrings #-}

module TestData where

import Control.Lens
import Types
import Data.ByteString as B

s1 :: B.ByteString
s1 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
s2 :: B.ByteString
s2 = "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
s3 :: B.ByteString
s3 = "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"

s4 :: B.ByteString
s4 = "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
s5 :: B.ByteString
s5 = "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
s6 :: B.ByteString
s6 = "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

c1 :: Card
c1 =
    card
        & cardID .~ 1
        & winning .~ [41, 48, 83, 86, 17]
        & having .~ [83, 86, 6, 31, 17, 9, 48, 53]

c2 :: Card
c2 =
    card
        & cardID .~ 2
        & winning .~ [13, 32, 20, 16, 61]
        & having .~ [61, 30, 68, 82, 17, 32, 24, 19]

c3 :: Card
c3 =
    card
        & cardID .~ 3
        & winning .~ [1, 21, 53, 59, 44]
        & having .~ [69, 82, 63, 72, 16, 21, 14, 1]

c4 :: Card
c4 =
    card
        & cardID .~ 4
        & winning .~ [41, 92, 73, 84, 69]
        & having .~ [59, 84, 76, 51, 58, 5, 54, 83]

c5 :: Card
c5 =
    card
        & cardID .~ 5
        & winning .~ [87, 83, 26, 28, 32]
        & having .~ [88, 30, 70, 12, 93, 22, 82, 36]

c6 :: Card
c6 =
    card
        & cardID .~ 6
        & winning .~ [31, 18, 13, 56, 72]
        & having .~ [74, 77, 10, 23, 35, 67, 36, 11]

cards :: [Card]
cards = [c1, c2, c3, c4, c5, c6]
