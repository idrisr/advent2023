{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.ByteString.Char8
import Game

data Color a = Red a | Blue a | Green a

getGameID :: Parser Int
getGameID = do
    _ <- string "Game "
    d <- decimal
    _ <- char ':'
    _ <- space
    pure d

getColor :: Parser (Color Int)
getColor = do
    d <- decimal
    _ <- space
    c <- choice [string "red", string "green", string "blue"]
    pure $ case c of
        "red" -> Red d
        "blue" -> Blue d
        "green" -> Green d
        _ -> undefined

makeRound :: Round -> [Color Int] -> Round
makeRound = foldl f
  where
    f :: Round -> Color Int -> Round
    f r (Red a) = r{red = a}
    f r (Green a) = r{green = a}
    f r (Blue a) = r{blue = a}

emptyRound :: Round
emptyRound = Round{red = 0, green = 0, blue = 0}

getRound :: Parser Round
getRound = do
    xs <- getColor `sepBy'` string ", "
    pure $ makeRound emptyRound xs

getRounds :: Parser [Round]
getRounds = getRound `sepBy` string "; "

getGame :: Parser Game
getGame = do
    a <- getGameID
    b <- getRounds
    pure $ Game{gameID = a, rounds = b}
