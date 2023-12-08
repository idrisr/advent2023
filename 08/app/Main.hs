{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad.State.Lazy
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import Data.Maybe
import Params
import Parse

type Node = ByteString
type MyState = (Directions, NetworkMap, Node)
type MyState2 = (Directions, NetworkMap, [Node])

main :: IO ()
main = do
    params <- cliParser -- get params
    t <- fileReader params -- read file
    (d, n) <- doParse t -- parse file
    print $ traverseMap2 1 (infBS d, n, ["AAA", "SJA", "BXA","QTA", "HCA", "LDA" ])

-- hack. couldnt figure out cycle
infBS :: ByteString -> ByteString
infBS = B.pack . concat . replicate 1000000 . B.unpack

-- s -> (a, s)

myState2 :: State MyState2 ()
myState2 = state thing
  where
    thing :: (Directions, NetworkMap, [Node]) -> ((), (Directions, NetworkMap, [Node]))
    thing (d, m, n) = ((), (B.tail d, m, fmap go n))
      where
        go z = case B.head d of
            76 -> fst . fromJust $ Map.lookup z m -- L
            _ -> snd . fromJust $ Map.lookup z m -- R

traverseMap2 :: Int -> MyState2 -> Int
traverseMap2 i s =
    let (_, (d, m, n)) = runState myState2 s
     in if and $ endsInZ <$> n
            then i
            else traverseMap2 (i + 1) (d, m, n)

endsInZ :: ByteString -> Bool
endsInZ b = B.last b == 90

myState :: State MyState ()
myState = state thing
    where
        thing :: (Directions, NetworkMap, Node) -> ((), (Directions, NetworkMap, Node))
        thing (d, m, n) = ((), (B.tail d, m, n1))
          where
            n1 = case B.head d of
                76 -> fst . fromJust $ Map.lookup n m -- L
                _ -> snd . fromJust $ Map.lookup n m -- R

traverseMap :: Int -> MyState -> Int
traverseMap i s =
    let (_, (d, m, n)) = runState myState s
     in if n == "ZZZ"
            then i
            else traverseMap (i + 1) (d, m, n)

fileReader :: Params -> IO ByteString
fileReader p =
    case p ^. input of
        FileInput f -> B.readFile f
        StdInput -> B.getContents

doParse :: ByteString -> IO (Directions, NetworkMap)
doParse t = case P.parseOnly parseFile t of
    Left err -> print err >> undefined
    Right h -> pure h
