{-# LANGUAGE OverloadedStrings#-}

module Parse where

import Prelude hiding (take, getLine)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.Map.Strict as Map

parseHeader :: Parser ByteString
parseHeader = P.takeWhile1 $ \c -> c == 'L' || c == 'R'

parseLine :: Parser (ByteString, (ByteString, ByteString))
parseLine = do
    k <- P.take 3
    l <- P.take 4 >> P.take 3
    r <- P.take 2 >> P.take 3
    _ <- P.take 1 >> P.takeWhile1 P.isSpace
    pure (k, (l, r))

type Directions = ByteString
type NetworkMap = Map.Map ByteString (ByteString, ByteString)

parseFile :: Parser (Directions, NetworkMap)
parseFile = do
    h <- parseHeader
    ls <- P.takeWhile1 isSpace >> P.many1 parseLine
    pure (h, Map.fromList ls)
