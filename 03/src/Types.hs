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
    }
    deriving (Show, Eq)

data Number = Number
    { _startIndex :: Int
    , _endIndex :: Int
    , _numValue :: Int
    , _symbolAdjacent :: Bool
    }
    deriving (Show, Eq)

data Symbol = Symbol
    { _symValue :: Char
    , _symIndex :: Int
    }
    deriving (Show, Eq)

number :: Number
number = Number 0 0 0 False
line :: Line
line = Line [] []
symbol :: Symbol
symbol = Symbol '.' 0

makeLenses ''Schematic
makeLenses ''Line
makeLenses ''Number
makeLenses ''Symbol

type Above = Line
type Current = Line
type Below = Line
