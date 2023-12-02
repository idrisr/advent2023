{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.Char
import Data.Maybe

getNumbers :: Parser [Maybe Int] -> B.ByteString -> [Int]
getNumbers p b =
    case parseOnly p b of
        Left _ -> []
        Right xs -> catMaybes xs

parseFLine :: Parser [Maybe Int]
parseFLine = many1 $ parseInt <|> parseNumber id <|> parseChar

parseRLine :: Parser [Maybe Int]
parseRLine = many1 $ parseInt <|> parseNumber B.reverse <|> parseChar

parseNumber :: (B.ByteString -> B.ByteString) -> Parser (Maybe Int)
parseNumber f =
    Just 1 <$ string (f "one")
        <|> Just 2 <$ string (f "two")
        <|> Just 3 <$ string (f "three")
        <|> Just 4 <$ string (f "four")
        <|> Just 5 <$ string (f "five")
        <|> Just 6 <$ string (f "six")
        <|> Just 7 <$ string (f "seven")
        <|> Just 8 <$ string (f "eight")
        <|> Just 9 <$ string (f "nine")

parseInt :: Parser (Maybe Int)
parseInt = do
    a <- digit
    pure . Just . digitToInt $ a

parseChar :: Parser (Maybe Int)
parseChar = letter_ascii >> pure Nothing
