module TestData where

import Types
import Control.Lens

s1 :: String
s1 = "467..114.."
line1 :: Line
line1 =
    line
        & numbers
            .~ [ number & startIndex .~ 0 & endIndex .~ 2 & numValue .~ 467
               , number & startIndex .~ 5 & endIndex .~ 7 & numValue .~ 114
               ]

s2 :: String
--    0123456789
s2 = "*.35.+633."
line2 :: Line
line2 =
    line
        & numbers
            .~ [ number & startIndex .~ 2 & endIndex .~ 3 & numValue .~ 35
               , number & startIndex .~ 6 & endIndex .~ 8 & numValue .~ 633
               ]
        & symbols
            .~ [ symbol & symIndex .~ 0 & symValue .~ '*'
               , symbol & symIndex .~ 5 & symValue .~ '+'
               ]

s3 :: String
--    0123456789
s3 = "...*......"
line3 :: Line
line3 =
    line
        & numbers
            .~ []
        & symbols
            .~ [ symbol & symIndex .~ 3 & symValue .~ '*'
               ]

s4 :: String
--    0123456789
s4 = ".........."
line4 :: Line
line4 = line & numbers .~ [] & symbols .~ [ ]
