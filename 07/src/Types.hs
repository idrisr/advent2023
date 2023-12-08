{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.List
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens

newtype Label = Label Text
    deriving (Eq, Show)

instance Ord Label where
    l1 <= l2 = labelVal l1 <= labelVal l2

labelVal :: Label -> Int
labelVal (Label a) = foldl f 0 xs
  where
    xs = zip (reverse s) [0 ..]
    s = T.unpack a
    f :: Int -> (Char, Int) -> Int
    f b (c, p) = sortValue2 c * 14 ^ p + b

sortValue :: Char -> Int
sortValue 'A' = 14
sortValue 'K' = 13
sortValue 'Q' = 12
sortValue 'J' = 11
sortValue 'T' = 10
sortValue a = digitToInt a

sortValue2 :: Char -> Int
sortValue2 'A' = 14
sortValue2 'K' = 13
sortValue2 'Q' = 12
sortValue2 'T' = 11
sortValue2 'J' = 2
sortValue2 a = digitToInt a + 1

data Hand = Hand
    { _label :: Label
    , _bid :: Int
    }
    deriving (Eq, Show)

makeLenses ''Hand

data Type
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfKind
    | FullHouse
    | FourOfKind
    | FiveOfKind
    deriving (Eq, Ord, Show)

instance Ord Hand where
    (Hand l1 _) <= (Hand l2 _) =
        if t1 == t2
        then l1 <= l2
        else t1 <= t2
        where t1 = handType2 l1
              t2 = handType2 l2

tSort :: Text -> String
tSort = sort . T.unpack

handType2 :: Label -> Type
handType2 (Label l) = case (groups, js) of
    ([5], 0) -> FiveOfKind
    ([4], 1) -> FiveOfKind
    ([3], 2) -> FiveOfKind
    ([2], 3) -> FiveOfKind
    ([1], 4) -> FiveOfKind
    ([0], 5) -> FiveOfKind

    ([1, 4], 0) -> FourOfKind
    ([1, 3], 1) -> FourOfKind
    ([1, 2], 2) -> FourOfKind
    ([1, 1], 3) -> FourOfKind
    ([0, 1], 4) -> FiveOfKind

    ([2, 3], 0) -> FullHouse
    ([2, 2], 1) -> FullHouse

    ([1, 1, 3], 0) -> ThreeOfKind
    ([1, 1, 2], 1) -> ThreeOfKind
    ([1, 1, 1], 2) -> ThreeOfKind

    ([1, 2, 2], 0) -> TwoPair
    ([1, 1, 2], 1) -> ThreeOfKind
    ([1, 1, 2], 2) -> FourOfKind

    ([1, 1, 1, 2], 0) -> OnePair
    ([1, 1, 1, 1], 1) -> OnePair

    (_, 1) -> OnePair
    (_, 2) -> ThreeOfKind
    (_, 3) -> FourOfKind
    (_, 4) -> FiveOfKind
    (_, 5) -> FiveOfKind
    _  -> HighCard
  where
    groups = sort . fmap length $ group . sort $ nojs
    js = length $ filter (== 'J') $ T.unpack l
    nojs = filter (/= 'J') $ T.unpack l

handType :: Label -> Type
handType (Label l) = case groups of
    [5] -> FiveOfKind
    [1, 4] -> FourOfKind
    [2, 3] -> FullHouse
    [1, 1, 3] -> ThreeOfKind
    [1, 2, 2] -> TwoPair
    [1, 1, 1, 2] -> OnePair
    _ -> HighCard
  where
    groups = sort . fmap length $ group . tSort $ l

winningsP1 :: [Hand] -> Int
winningsP1 hs = sum $ fmap f xs
  where
    xs = sort hs `zip` [1..]
    f (Hand _ b, r) = b * r

winningsP2 :: [Hand] -> Int
winningsP2 hs = sum $ fmap f xs
  where
    xs = sort hs `zip` [1..]
    f (Hand _ b, r) = b * r

debug :: [Hand] -> [Hand]
debug = sort
