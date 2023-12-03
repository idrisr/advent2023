{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

newtype Schematic = Schematic
    { _lines :: [Line]
    }
    deriving (Show)

data Line = Line
    { _numbers :: [Number]
    , _symbols :: [Symbol]
    , _stars:: [Star]
    }
    deriving (Show, Eq)

data Number = Number
    { _startIndex :: Int
    , _endIndex :: Int
    , _numValue :: Int
    , _symbolAdjacent :: Bool
    }
    deriving (Show, Eq)

data Star = Star
    { _isGear :: Bool
    , _starIndex :: Int
    , _gearValues :: (Int, Int)
    }
    deriving (Show, Eq)

data Symbol = Symbol
    { _symValue :: Char
    , _symIndex :: Int
    }
    deriving (Show, Eq)

makeLenses ''Schematic
makeLenses ''Line
makeLenses ''Number
makeLenses ''Symbol
makeLenses ''Star

starGearValue :: Lens' Star Int
starGearValue = lens g setter
  where
    g s = if s ^. isGear then s ^. (gearValues . _1) * s ^. (gearValues . _2) else 0
    setter = const

numberSymbolValue :: Lens' Number Int
numberSymbolValue = lens g s
  where
    g n = if n ^. symbolAdjacent
            then n ^. numValue
            else 0
    s = const

number :: Number
number = Number 0 0 0 False
line :: Line
line = Line [] [] []
symbol :: Symbol
symbol = Symbol '.' 0
star :: Star
star = Star False 0 (0, 0)

type Above = Line
type Current = Line
type Below = Line
