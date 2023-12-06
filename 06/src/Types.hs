module Types where

type RaceTime = Integer
type RaceRecord = Integer

quadratic :: RaceTime -> RaceRecord -> (Double, Double)
quadratic t r = (l, h)
  where
    b = fromIntegral t
    c = fromIntegral r + 0.0001
        -- smudge factor to avoid false positive where distance equals record
    l = ((-b) + det) / (-2)
    h = ((-b) - det) / (-2)
    det = sqrt (b * b - 4 * c)

solutions :: RaceTime -> RaceRecord -> Int
solutions t r = floor b - ceiling a + 1
    where (a, b) = quadratic t r
