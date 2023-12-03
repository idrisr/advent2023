{-# LANGUAGE OverloadedStrings #-}

module TestData where

import qualified Data.ByteString as B
import Lens.Micro.Platform
import Game
import Prelude hiding (round)

game1BS :: B.ByteString
game1BS = "Game 1: 4 red, 3 blue; 1 red, 2 green, 6 blue; 2 green"
game1G :: Game
game1G =
    game
        & gameID .~ 1
        & rounds
            .~ [ round & red .~ 4 & green .~ 0 & blue .~ 3
               , round & red .~ 1 & green .~ 2 & blue .~ 6
               , round & red .~ 0 & green .~ 2 & blue .~ 0
               ]

game2BS :: B.ByteString
game2BS = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
game2G :: Game
game2G =
    game
        & gameID .~ 2
        & rounds
            .~ [ round & red .~ 0 & green .~ 2 & blue .~ 1
               , round & red .~ 1 & green .~ 3 & blue .~ 4
               , round & red .~ 0 & green .~ 1 & blue .~ 1
               ]

game3BS :: B.ByteString
game3BS = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
game3G :: Game
game3G =
    game
        & gameID .~ 3
        & rounds
            .~ [ round & red .~ 20 & green .~ 8 & blue .~ 6
               , round & red .~ 4 & green .~ 13 & blue .~ 5
               , round & red .~ 1 & green .~ 5 & blue .~ 0
               ]

game4BS :: B.ByteString
game4BS = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
game4G :: Game
game4G =
    game
        & gameID .~ 4
        & rounds
            .~ [ round & red .~ 3 & green .~ 1 & blue .~ 6
               , round & red .~ 6 & green .~ 3 & blue .~ 0
               , round & red .~ 14 & green .~ 3 & blue .~ 15
               ]

game5BS :: B.ByteString
game5BS = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
game5G :: Game
game5G =
    game
        & gameID .~ 5
        & rounds
            .~ [ round & red .~ 6 & green .~ 3 & blue .~ 1
               , round & red .~ 1 & green .~ 2 & blue .~ 2
               ]
