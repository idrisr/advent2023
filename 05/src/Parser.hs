{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.ByteString.Char8
import Types
import Control.Lens
import qualified Data.ByteString as B

getSeeds :: Parser [Seed]
getSeeds = do
    _ <- string "seeds: "
    ss <- decimal `sepBy1` many1 space
    _ <- many' endOfLine
    pure $ fmap Seed ss

getRange :: Parser Range
getRange =
    let range = Range 0 0 0
     in do
            d <- decimal
            _ <- space
            s <- decimal
            _ <- space
            r <- decimal
            _ <- many' endOfLine
            pure $ range & dest .~ d & source .~ s & rangeLength .~ r

getMap :: B.ByteString -> Parser [Range]
getMap b = do
    _ <- string b >> many' endOfLine
    rs <- many1 getRange
    _ <- many' endOfLine
    pure rs

getFarmMap :: Parser (FarmMap, [Seed])
getFarmMap =
    let farmMapEmpty = FarmMap [] [] [] [] [] [] []
     in do
            seeds <- getSeeds
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
            pure (fm, seeds)
