module Day01 where

import System.Environment
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Parser

part2 :: B.ByteString -> Int
part2 s = 10 * head fs + head rs
  where
    fs = getNumbers parseFLine s
    rs = getNumbers parseRLine $ B.reverse s

part1 :: B.ByteString -> Int
part1 s = 10 * head fs + last fs
  where
    fs = getNumbers parseDigits s

runSum :: (B.ByteString -> Int) -> IO ()
runSum f =
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
