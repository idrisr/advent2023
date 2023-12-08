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

main :: IO ()
main = do
    params <- cliParser -- get params
    t <- fileReader params -- read file
    (d, n) <- doParse t -- parse file

    case params^.part of
        Part1 -> print $ traverseMap allZ 1 (infBS d, n, "AAA")
        Part2 -> do
                 let f node = traverseMap endsInZ 1 (infBS d, n, node)
                 let nodes = filter endsInA (Map.keys n)
                 print $ foldl lcm 1 $ f <$> nodes

infBS :: ByteString -> ByteString
-- hack!!
infBS = B.pack . concat . replicate 80 . B.unpack

endsInZ :: ByteString -> Bool
endsInZ b = B.last b == 90

endsInA :: ByteString -> Bool
endsInA b = B.last b == 65

allZ :: ByteString -> Bool
allZ b = b == "ZZZ"

myState :: State MyState ()
myState = state thing
    where
        thing :: (Directions, NetworkMap, Node) -> ((), (Directions, NetworkMap, Node))
        thing (d, m, n) = ((), (B.tail d, m, n1))
          where
            n1 = case B.head d of
                76 -> fst . fromJust $ Map.lookup n m -- L
                _ -> snd . fromJust $ Map.lookup n m -- R

traverseMap :: (ByteString -> Bool) -> Int -> MyState -> Int
traverseMap f i s =
    let (_, (d, m, n)) = runState myState s
     in if f n
        then i
        else traverseMap f (i + 1) (d, m, n)

fileReader :: Params -> IO ByteString
fileReader p =
    case p ^. input of
        FileInput f -> B.readFile f
        StdInput -> B.getContents

doParse :: ByteString -> IO (Directions, NetworkMap)
doParse t = case P.parseOnly parseFile t of
    Left err -> print err >> undefined
    Right h -> pure h
