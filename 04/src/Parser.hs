{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.ByteString.Char8
import Types
import Control.Lens

getCard :: Parser Card
getCard = do
    a <- getCardID
    w <- getNumbers
    _ <- many1 space >> char '|' >> many1 space
    h <- getNumbers
    pure $
        card
            & cardID .~ a
            & winning .~ w
            & having .~ h

getCardID :: Parser Int
getCardID = do
    _ <- string "Card" >> many1 space
    d <- decimal
    _ <- char ':' >> many1 space
    pure d

getNumbers :: Parser [Int]
getNumbers = do
    decimal `sepBy1` many1 space
