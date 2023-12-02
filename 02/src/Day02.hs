module Day02 where

import Control.Exception
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Game
import Parser
import System.Environment

contents :: Contents
contents = Round{red = 12, green = 13, blue = 14}

possible :: B.ByteString -> Int
possible b = case game b of
    Left _ -> undefined
    Right g ->
        if gamePossible contents g
            then gameID g
            else 0
  where
    game = parseOnly getGame

runSum :: IO ()
runSum =
    catch
        ( do
            arg <- handleArgs
            case arg of
                Left err -> putStrLn $ "Error " <> err
                Right fname -> do
                    c <- B.lines <$> B.readFile fname
                    let s = sum $ fmap possible c
                    putStrLn $ "Total: " ++ show s
        )
        handleErr
  where
    handleErr :: IOError -> IO ()
    handleErr e = putStrLn "Error" >> print e

handleArgs :: IO (Either String FilePath)
handleArgs = do
    args <- getArgs
    pure $ case args of
        [f] -> Right f
        [] -> Left "must provide one input file"
        _ -> Left "must provide only one input file"
