module Main where

import Control.Arrow

main :: IO ()
main = do
    t <- getContents
    let ns = lines t
    print $ sum $ (parse >>> makeSequence >>> extrapolate1) <$> ns
    print $ sum $ (parse >>> makeSequence >>> extrapolate2) <$> ns

parse :: String -> [Int]
parse t = read <$> words t

diff :: [Int] -> [Int]
diff xs = [ b - a | (a, b) <- zip xs (tail xs)]

makeSequence :: [Int] -> [[Int]]
makeSequence = iterate diff >>> takeWhile (any (0/=))

extrapolate1 :: [[Int]] -> Int
extrapolate1 xs = sum $ last <$> xs

extrapolate2 :: [[Int]] -> Int
extrapolate2 xs = foldl (flip (-)) 0 $ head <$> reverse xs
