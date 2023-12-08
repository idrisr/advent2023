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

main :: IO ()
main = do
    params <- cliParser -- get params
    t <- fileReader params -- read file
    (d, n) <- doParse t -- parse file
    print $ go 1 (infBS d, n, "AAA")

infBS :: ByteString -> ByteString
infBS = B.pack . concat . replicate 10000 . B.unpack

type Node = ByteString
type MyState = (Directions, NetworkMap, Node)

myState :: State MyState ()
myState = state thing
    where
        thing :: (Directions, NetworkMap, Node) -> ((), (Directions, NetworkMap, Node))
        thing (d, m, n) = ((), (B.tail d, m, n1))
          where
            n1 = case B.head d of
                76 -> fst . fromJust $ Map.lookup n m -- L
                _ -> snd . fromJust $ Map.lookup n m -- R

go :: Int -> MyState -> Int
go i s =
    let (_, (d, m, n)) = runState myState s
     in if n == "ZZZ"
            then i
            else go (i + 1) (d, m, n)

fileReader :: Params -> IO ByteString
fileReader p =
    case p ^. input of
        FileInput f -> B.readFile f
        StdInput -> B.getContents

doParse :: ByteString -> IO (Directions, NetworkMap)
doParse t = case P.parseOnly parseFile t of
    Left err -> print err >> undefined
    Right h -> pure h
