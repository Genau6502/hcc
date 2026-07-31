module Main where

import System.Environment (getArgs)
import Tokeniser
import Parser
import Registers

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> do
            input <- readFile inputFile
            putStrLn $ show (tokenise input)
        _ -> do
            putStrLn "[Error] 1 argument <input_file> required"    
