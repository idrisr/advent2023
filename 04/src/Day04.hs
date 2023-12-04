module Day04 (runPart1, runPart2) where

import Control.Exception
import Control.Lens
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Parser
import System.Environment
import Types

process1 :: B.ByteString -> IO Int
process1 b = case parseOnly getCard b of
    Left e -> B.putStrLn b >> putStrLn e >> pure 0
    Right g -> pure $ g ^. copies

process2 :: B.ByteString -> Card
process2 b = case parseOnly getCard b of
    Left _ -> undefined
    Right g -> g

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
                c <- B.lines <$> B.readFile fname
                s <- mapM process1 c
                putStrLn $ "Total: " ++ show (sum s)

runPart2 :: IO ()
runPart2 = catch processFile handleErr
  where
    handleErr :: IOError -> IO ()
    handleErr e = putStrLn "Error" >> print e
    processFile = do
        arg <- handleArgs
        case arg of
            Left err -> putStrLn $ "Error: " <> err
            Right fname -> do
                c <- B.lines <$> B.readFile fname
                let s = updateCards $ process2 <$> c
                let s1 = (^. copies) <$> s
                putStrLn $ "Total: " ++ show (sum s1)

handleArgs :: IO (Either String FilePath)
handleArgs = do
    args <- getArgs
    pure $ case args of
        [f] -> Right f
        [] -> Left "Error: must provide one input file"
        _ -> Left "Error: must provide only one input file"
