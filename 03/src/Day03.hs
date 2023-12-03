module Day03 where

import Control.Exception
import System.Environment
import Schematic
import Types

asdf :: [String] -> Int
asdf xs = totalSchematic schematic
    where ls = fmap makeLine xs
          schematic = Schematic $ updateLines ls

runPart :: IO ()
runPart =
    catch
        ( do
            arg <- handleArgs
            case arg of
                Left err -> putStrLn $ "Error " <> err
                Right fname -> do
                    c <- Prelude.lines <$> readFile fname
                    let s = asdf c
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
