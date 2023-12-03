module Day02 where

import Control.Exception
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Game
import Lens.Micro.Platform
import Parser
import System.Environment

possible :: B.ByteString -> Int
possible b = case parseOnly getGame b of
    Left _ -> undefined
    Right g ->
        if gamePossible contents g
            then g ^. gameID
            else 0

powerB :: B.ByteString -> Int
powerB b = case parseOnly getGame b of
    Left _ -> undefined
    Right g -> power . minContents $ g

runPart :: (B.ByteString -> Int) -> IO ()
runPart f =
    catch
        ( do
            arg <- handleArgs
            case arg of
                Left err -> putStrLn $ "Error " <> err
                Right fname -> do
                    c <- B.lines <$> B.readFile fname
                    let s = sum $ fmap f c
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
