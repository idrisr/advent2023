{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.ByteString.Char8
import Types
import Control.Lens
import Data.List.Split
import qualified Data.ByteString as B

getSeedRange :: Parser [SeedRange]
getSeedRange = do
    ss <- getSeeds
    let xs = chunksOf 2 ss
    let
        f [a, b] = SeedRange a b
        f _ = undefined
    pure $ fmap f xs

getSeeds :: Parser [Seed]
getSeeds = do
    ss <- string "seeds: " >> decimal `sepBy1` many1 space
    _ <- many' endOfLine
    pure $ fmap Seed ss

getRange :: Parser Range
getRange =
    let range = Range 0 0 0
     in do
            d <- decimal
            s <- space >> decimal
            r <- space  >> decimal
            _ <- many' endOfLine
            pure $ range & dest .~ d & source .~ s & rangeLength .~ r

getMap :: B.ByteString -> Parser [Range]
getMap b = do
    _ <- string b >> many' endOfLine
    rs <- many1 getRange
    _ <- many' endOfLine
    pure rs

getFarmMapPart1 :: Parser (FarmMap, [Seed])
getFarmMapPart1 = do
    seeds <- getSeeds
    fm <- getFarmMap
    pure (fm, seeds)

getFarmMapPart2 :: Parser (FarmMap, [SeedRange])
getFarmMapPart2 = do
    seedRanges <- getSeedRange
    fm <- getFarmMap
    pure (fm, seedRanges)

getFarmMap :: Parser FarmMap
getFarmMap =
    let farmMapEmpty = FarmMap [] [] [] [] [] [] []
     in do
            r1 <- getMap "seed-to-soil map:"
            r2 <- getMap "soil-to-fertilizer map:"
            r3 <- getMap "fertilizer-to-water map:"
            r4 <- getMap "water-to-light map:"
            r5 <- getMap "light-to-temperature map:"
            r6 <- getMap "temperature-to-humidity map:"
            r7 <- getMap "humidity-to-location map:"
            let fm =
                    farmMapEmpty
                        & seed2soilR .~ r1
                        & soil2fertR .~ r2
                        & fert2waterR .~ r3
                        & water2lightR .~ r4
                        & light2tempR .~ r5
                        & temp2humidR .~ r6
                        & humid2locaR .~ r7
            pure fm
