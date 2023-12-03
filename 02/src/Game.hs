{-# LANGUAGE TemplateHaskell #-}

module Game where

import Lens.Micro.Platform
import Prelude hiding (round)

data Game = Game
    { _gameID :: Int
    , _rounds :: [Round]
    }
    deriving (Eq, Show)

data Round = Round
    { _red :: Int
    , _blue :: Int
    , _green :: Int
    }
    deriving (Eq, Show)

makeLenses ''Game
makeLenses ''Round

type Contents = Round
type Cubeset = Round

round :: Round
round = Round 0 0 0

game :: Game
game = Game 0 mempty

contents :: Contents
contents = round & red .~ 12 & green .~ 13 & blue .~ 14

gamePossible :: Contents -> Game -> Bool
gamePossible c g = all (roundPossible c) $ _rounds g

-- traversal? fold?
minContents :: Game -> Cubeset
minContents z = round & red .~ f red
                       & green .~ f green
                       & blue .~ f blue
  where
    f l = maximum $ z ^. rounds ^.. traverse . l

roundPossible :: Contents -> Round -> Bool
roundPossible c r =
    c ^. red >= r ^. red
        && c ^. green >= r ^. green
        && c ^. blue >= r ^. blue

power :: Cubeset -> Int
power g = g^.red * g^.blue * g^.green
