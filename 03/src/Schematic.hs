module Schematic where

import Types
import Control.Lens
import Prelude hiding (lines)
import Data.List (groupBy, findIndices)
import Data.Char (isNumber)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List.Split

-- sum all lines in schematic
totalSchematic :: Schematic -> Int
totalSchematic s = sum $ fmap totalLine $ s^.lines

-- used to feed into updateLine
padLines :: [Line] -> [(Maybe Line, Line, Maybe Line)]
padLines ls = fmap f $ divvy 3 1 $ Nothing : (fmap Just ls ++ [Nothing])
  where
    f [a, Just b, c] = (a, b, c)
    f _ = undefined

-- sum numbers with adjacent symbols in line
totalLine :: Line -> Int
totalLine l = sum $ fmap go $ l^.numbers
  where
    go n = if n^.symbolAdjacent then n^.numValue else 0

updateLines :: [Line] -> [Line]
updateLines xs = fmap f ys
  where
    ys = padLines xs
    f (a, b, c) = updateLine a b c

updateLine :: Maybe Above -> Current -> Maybe Below -> Line
updateLine Nothing c Nothing = c
updateLine Nothing c (Just b) = c & numbers .~ updateNumbers (c ^. numbers) (b ^. symbols ++ c ^. symbols)
updateLine (Just a) c Nothing = c & numbers .~ updateNumbers (c ^. numbers) (a ^. symbols ++ c ^. symbols)
updateLine (Just a) c (Just b) = c & numbers .~ updateNumbers (c ^. numbers) (a ^. symbols ++ b ^. symbols ++ c ^. symbols)

updateNumbers :: [Number] -> [Symbol] -> [Number]
updateNumbers ns xs = fmap (`updateNumber` xs) ns

updateNumber :: Number -> [Symbol] -> Number
updateNumber n xs = n & symbolAdjacent .~ b
  where
    b = any (isAdjacent n) xs

isAdjacent :: Number -> Symbol -> Bool
isAdjacent n x = symIdx >= minIdx && symIdx <= maxIdx
  where
    -- +-1 to account for diagonals
    -- dont need to worry about out of bounds
    -- cause no list indexing actually happening
    minIdx = n ^. startIndex - 1
    maxIdx = n ^. endIndex + 1
    symIdx = x ^.symIndex

makeLine :: String -> Line
makeLine s =
    line
        & numbers .~ fmap (uncurry mkNumber) (zip values numberIndices)
        & symbols .~ fmap (uncurry mkSymbol) (zip symbolValues symbolIndices)
  where
    numberIndices = fmap f $ consec $ findIndices isNumber s
      where
        f xs = (head xs, last xs)
    values :: [Int]
    values = mapMaybe readMaybe $ splitWhen (not . isNumber) s
    mkNumber :: Int -> (Int, Int) -> Number
    mkNumber v (start, end) =
        number
            & startIndex .~ start
            & endIndex .~ end
            & numValue .~ v
    symbolIndices = findIndices isSymbol s
    symbolValues = filter isSymbol s
    mkSymbol :: Char -> Int -> Symbol
    mkSymbol c i = symbol
        & symValue .~ c
        & symIndex .~ i

consec :: (Enum a, Num a, Eq a) => [a] -> [[a]]
consec = fmap (fmap snd) . groupBy f . zip [0 ..]
  where
    f (i, a) (j, b) = a + j == b + i

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && (not . isNumber) c
