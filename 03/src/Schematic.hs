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
totalSchematic s = sum $ go <$> s ^. lines
  where
    go :: Line -> Int
    go l = sum $ (^. numberSymbolValue) <$> l ^. numbers

totalGears :: Schematic -> Int
totalGears s = sum $ go <$> s ^. lines
  where
    -- sum gear ratios
    go :: Line -> Int
    go l = sum $ (^. starGearValue) <$> l ^. stars

-- used to feed into updateLine
padLines :: [Line] -> [(Maybe Line, Line, Maybe Line)]
padLines ls = f <$> divvy 3 1 ms
  where
    f [a, Just b, c] = (a, b, c)
    f _ = undefined
    ms = Nothing : (fmap Just ls ++ [Nothing])

updateLines :: [Line] -> [Line]
updateLines xs = f <$> ys
  where
    ys = padLines xs
    f (a, b, c) = updateLine a b c

updateLine :: Maybe Above -> Current -> Maybe Below -> Line
updateLine Nothing c Nothing = c
updateLine Nothing c (Just b) = c
        & numbers %~ (`updateNumbers` (b ^. symbols ++ c ^. symbols))
        & stars %~ (`updateStars` (b ^. numbers ++ c ^. numbers))
updateLine (Just a) c Nothing = c
        & numbers %~ (`updateNumbers` (a ^. symbols ++ c ^. symbols))
        & stars %~ (`updateStars` (a ^. numbers ++ c ^. numbers))
updateLine (Just a) c (Just b) = c
        & numbers %~ (`updateNumbers` (a ^. symbols ++ b ^. symbols ++ c ^. symbols))
        & stars %~ (`updateStars` (a ^. numbers ++ b ^. numbers ++ c ^. numbers))

update2ndPass :: (a -> [b] -> a) -> [a] -> [b] -> [a]
update2ndPass f ns xs = (`f` xs) <$> ns

updateNumbers :: [Number] -> [Symbol] -> [Number]
updateNumbers = update2ndPass updateNumber

updateStars :: [Star] -> [Number] -> [Star]
updateStars = update2ndPass updateStar

updateNumber :: Number -> [Symbol] -> Number
updateNumber n xs = n & symbolAdjacent .~ b
  where
    b = any (isSymbolAdjacent n) xs

updateStar :: Star -> [Number] -> Star
updateStar s ns =
    if length b == 2
        then s
            & isGear .~ True
            & gearValues .~ (head b^.numValue, last b^.numValue)
        else s
  where
    b = filter (`isStarAdjacent` s) ns

isStarAdjacent :: Number -> Star -> Bool
isStarAdjacent n s = symIdx >= minIdx && symIdx <= maxIdx
  where
    minIdx = n ^. startIndex - 1
    maxIdx = n ^. endIndex + 1
    symIdx = s ^.starIndex

isSymbolAdjacent :: Number -> Symbol -> Bool
isSymbolAdjacent n x = symIdx >= minIdx && symIdx <= maxIdx
  where
    -- +-1 to account for diagonals , dont need to worry about out of bounds , cause no list indexing actually happening
    minIdx = n ^. startIndex - 1
    maxIdx = n ^. endIndex + 1
    symIdx = x ^.symIndex

makeLine :: String -> Line
makeLine s =
    line
        & numbers .~ fmap (uncurry mkNumber) (zip values numberIndices)
        & symbols .~ zs
        & stars .~ fmap mkStar (filter (\x -> x ^. symValue == '*') zs)
  where
    zs = fmap (uncurry mkSymbol) (zip symbolValues symbolIndices)
    numberIndices = f <$> consec (findIndices isNumber s)
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
    mkSymbol c i =
        symbol
            & symValue .~ c
            & symIndex .~ i
    mkStar :: Symbol -> Star
    mkStar sym = star & starIndex .~ sym ^. symIndex

consec :: (Enum a, Num a, Eq a) => [a] -> [[a]]
consec = fmap (fmap snd) . groupBy f . zip [0 ..]
  where
    f (i, a) (j, b) = a + j == b + i

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && (not . isNumber) c
