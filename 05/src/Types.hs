{-# LANGUAGE TemplateHaskell #-}

module Types where
import Control.Lens
import Data.Maybe

newtype Seed = Seed Int deriving (Eq, Ord, Show)
newtype Soil = Soil Int deriving (Eq, Ord, Show)
newtype Fertilizer = Fertilizer Int deriving (Eq, Ord, Show)
newtype Water = Water Int deriving (Eq, Ord, Show)
newtype Light = Light Int deriving (Eq, Ord, Show)
newtype Temperature = Temperature Int deriving (Eq, Ord, Show)
newtype Humidity = Humidity Int deriving (Eq, Ord, Show)
newtype Location = Location Int deriving (Eq, Ord, Show)

data FarmMap = FarmMap
    { _seed2soilR :: [Range]
    , _soil2fertR :: [Range]
    , _fert2waterR :: [Range]
    , _water2lightR :: [Range]
    , _light2tempR :: [Range]
    , _temp2humidR :: [Range]
    , _humid2locaR :: [Range]
    }
    deriving (Show)

farmMapEmpty :: FarmMap
farmMapEmpty = FarmMap [] [] [] [] [] [] []

data Range = Range {
    _dest :: Int,
    _source :: Int,
    _rangeLength :: Int
} deriving Show

range :: Range
range = Range 0 0 0

makeLenses ''Range
makeLenses ''FarmMap

minValueR :: Getter Range Int
minValueR = lens (^.source) const

maxValueR :: Getter Range Int
maxValueR = lens (\r -> r^.source + r^.rangeLength - 1) const

seedToSoil :: FarmMap -> Seed -> Soil
seedToSoil f (Seed a) = Soil $ farmMap a (f^.seed2soilR)

soilToFert :: FarmMap -> Soil -> Fertilizer
soilToFert f (Soil a) = Fertilizer $ farmMap a (f^.soil2fertR)

fertToWater :: FarmMap -> Fertilizer -> Water
fertToWater f (Fertilizer a) = Water $ farmMap a $ f^.fert2waterR

water2light :: FarmMap -> Water -> Light
water2light f (Water a) = Light $ farmMap a $ f^.water2lightR

light2temp :: FarmMap -> Light -> Temperature
light2temp f (Light a) = Temperature $ farmMap a $ f^.light2tempR

temp2humid :: FarmMap -> Temperature -> Humidity
temp2humid f (Temperature a) = Humidity $ farmMap a $ f^.temp2humidR

humid2loca :: FarmMap -> Humidity -> Location
humid2loca f (Humidity a) = Location $ farmMap a $ f^.humid2locaR

-- use applicative reader?
seed2Location :: FarmMap -> Seed -> Location
seed2Location f =
    humid2loca f .
    temp2humid f .
    light2temp f .
    water2light f .
    fertToWater f .
    soilToFert f .
    seedToSoil f

farmMap :: Int -> [Range] -> Int
farmMap i rs = if null ys then i else head ys
  where
    ys = mapMaybe (inRange i) rs
    inRange :: Int -> Range -> Maybe Int
    inRange j r =
        if j >= r ^. minValueR && j <= r ^. maxValueR
            then Just $ j + r ^. dest - r ^. source
            else Nothing
