module Game where

data Game = Game
    { gameID :: Int
    , rounds :: [Round]
    }
    deriving (Eq, Show)


data Round = Round
    { red :: Int
    , blue :: Int
    , green :: Int
    }
    deriving (Eq, Show)

type Contents = Round

gamePossible :: Contents -> Game -> Bool
gamePossible c g = all (roundPossible c) $ rounds g

roundPossible :: Contents -> Round -> Bool
roundPossible c r =
    red c >= red r
        && green c >= green r
        && blue c >= blue r
