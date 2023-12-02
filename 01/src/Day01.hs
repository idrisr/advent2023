module Day01 where

import System.Environment
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Parser

firstLast :: B.ByteString -> Int
firstLast s = 10 * head fs + head rs
  where
    fs = getNumbers parseFLine s
    rs = getNumbers parseRLine $ B.reverse s

runSum :: IO ()
runSum =
    catch
        ( do
            arg <- handleArgs
            case arg of
                Left err -> putStrLn $ "Error " <> err
                Right fname -> do
                    c <- B.lines <$> B.readFile fname
                    let s = sum $ fmap firstLast c
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
