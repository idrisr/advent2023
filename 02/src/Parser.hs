{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.ByteString.Char8
import Game
import Lens.Micro.Platform
import Prelude hiding (round)

data Color a = Red a | Blue a | Green a

getGame :: Parser Game
getGame = do
    a <- getGameID
    b <- getRounds
    pure $
        game
            & gameID .~ a
            & rounds .~ b

getGameID :: Parser Int
getGameID = do
    _ <- string "Game "
    d <- decimal
    _ <- char ':'
    _ <- space
    pure d

getRound :: Parser Round
getRound = do
    xs <- getColor `sepBy'` string ", "
    pure $ makeRound round xs

getRounds :: Parser [Round]
getRounds = getRound `sepBy` string "; "

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
makeRound = foldl go
  where
    go :: Round -> Color Int -> Round
    go r (Red a) = r & red .~ a
    go r (Green a) = r & green .~ a
    go r (Blue a) = r & blue .~ a
