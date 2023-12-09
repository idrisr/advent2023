module Main where

import Control.Arrow

main :: IO ()
main = do
    t <- getContents
    let ns = lines t
    print $ sum $ part1 <$> ns
    print $ sum $ part2 <$> ns

common :: String -> [[Int]]
common = parse >>> makeSequence

part1 :: String -> Int
part1 = common >>> extrapolate1

part2 :: String -> Int
part2 = common >>> extrapolate2

parse :: String -> [Int]
parse t = read <$> words t

diff :: [Int] -> [Int]
diff xs = [ b - a | (a, b) <- zip xs (tail xs)]

allZero :: [Int] -> Bool
allZero = foldl (\b a -> a == 0 && b) True

makeSequence :: [Int] -> [[Int]]
makeSequence ys = go [ys]
  where
    go xs =
        let ds = diff $ last xs
         in if allZero ds then xs else go (xs ++ [ds])

extrapolate1 :: [[Int]] -> Int
extrapolate1 xs = sum $ last <$> xs

extrapolate2 :: [[Int]] -> Int
extrapolate2 xs = foldl (flip (-)) 0 $ head <$> reverse xs
