module Day05 (runPart1) where

import Control.Exception
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Parser
import System.Environment
import Types

process1 :: B.ByteString -> IO (FarmMap, [Seed])
process1 b = case parseOnly getFarmMap b of
    Left e -> B.putStrLn b >> putStrLn e >> pure (farmMapEmpty, [])
    Right a -> pure a

runPart1 :: IO ()
runPart1 = catch processFile handleErr
  where
    handleErr :: IOError -> IO ()
    handleErr e = putStrLn "Error" >> print e
    processFile = do
        arg <- handleArgs
        case arg of
            Left err -> putStrLn $ "Error: " <> err
            Right fname -> do
                c <- B.readFile fname
                (f, ss) <- process1 c
                let seed = minimum $ fmap (seed2Location f)  ss
                putStrLn $ "Minimum Seed: " ++ show seed

handleArgs :: IO (Either String FilePath)
handleArgs = do
    args <- getArgs
    pure $ case args of
        [f] -> Right f
        [] -> Left "Error: must provide one input file"
        _ -> Left "Error: must provide only one input file"
