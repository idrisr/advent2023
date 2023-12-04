{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.List

data Card = Card
    { _cardID :: Int
    , _winning :: [Int]
    , _having :: [Int]
    , _copies:: Int
    }
    deriving (Eq, Show)

card :: Card
card = Card 0 [] [] 1

makeLenses ''Card

-- unlawful
pointValue :: Lens' Card Int
pointValue = lens g s
  where
    g c =
        let l = length $ intersect (c ^. winning) (c ^. having)
         in if l > 0 then 2 ^ (l - 1) else 0
    s = const

-- unlawful
matches :: Lens' Card Int
matches = lens g s
  where
    g c = length $ intersect (c ^. winning) (c ^. having)
    s = const

updateCards :: [Card] -> [Card]
updateCards [] = []
updateCards (x : xs) =
    x : ys
  where
    (a, b) = splitAt (x^.matches) xs
    ys = updateCards $ (f <$> a) ++ b
    updateCard :: Int -> Card -> Card
    updateCard i c = c & copies +~ i
    f = updateCard $ x^.copies
