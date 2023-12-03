module Day03 where

import Control.Exception
import System.Environment
import Schematic
import Types

process :: [String] -> (Schematic -> Int) -> Int
process xs f = f schematic
  where
    ls = fmap makeLine xs
    schematic = Schematic $ updateLines ls

runPart :: (Schematic -> Int) -> IO ()
runPart f = catch processFile handleErr
  where
    handleErr :: IOError -> IO ()
    handleErr e = putStrLn "Error" >> print e
    processFile = do
        arg <- handleArgs
        case arg of
            Left err -> putStrLn $ "Error " <> err
            Right fname -> do
                c <- Prelude.lines <$> readFile fname
                putStrLn $ "Total: " ++ show (process c f)

handleArgs :: IO (Either String FilePath)
handleArgs = do
    args <- getArgs
    pure $ case args of
        [f] -> Right f
        [] -> Left "must provide one input file"
        _ -> Left "must provide only one input file"
