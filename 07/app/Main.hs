{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Params
import Parser
import Types

main :: IO ()
main = do
    params <- cliParser
    xs <- fileReader params
    hs <- mapM doParsey xs
    mapM_ print $ debug hs
    print $ winnings hs

fileReader :: Params -> IO [Text]
fileReader p =
    T.lines
        <$> case p ^. input of
            FileInput f -> T.readFile f
            StdInput -> T.getContents

doParsey :: Text -> IO Hand
doParsey t =
    case P.parseOnly Parser.getLine t of
        Left err -> print err >> undefined
        Right h -> pure h

cliParser :: IO Params
cliParser = execParser opts
  where
    opts =
        info
            (getParams <**> helper)
            ( fullDesc
                <> progDesc "Advent of Code"
                <> header "Command Line Runner for AOC 2023 Day 7"
            )
