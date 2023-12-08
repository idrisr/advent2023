{-# LANGUAGE TemplateHaskell #-}

module Params where

import Options.Applicative
import Control.Lens

data Part = Part1 | Part2

instance Show Part where
    show Part1 = "1"
    show Part2 = "2"

data Params = Params
    { _input :: Input
    , _part :: Part
    }

data Input = FileInput FilePath | StdInput

makeLenses ''Params

fileInput :: Parser Input
fileInput = FileInput <$> strOption
    ( long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")

stdInput :: Parser Input
stdInput = flag' StdInput ( long "stdin" <> help "Read from stdin")

partReader :: ReadM Part
partReader = maybeReader go
  where
    go "1" = Just Part1
    go "2" = Just Part2
    go _ = Nothing

getInput :: Parser Input
getInput = fileInput <|> stdInput

getParams :: Parser Params
getParams =
    Params
        <$> getInput
        <*> option
            partReader
            ( long "part"
                <> help "which part to run, 1 or 2"
                <> short 'p'
                <> showDefault
                <> value Part1
                <> metavar "INT"
            )
