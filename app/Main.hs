module Main where

import System.Environment (getArgs)
import Tokeniser
import Parser
import Instructions
import Compile
import Types

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> compileFile inputFile outputFile
        _ -> do
            putStrLn "[Error] 2 arguments <input_file> <output_file> required"

compileFile :: String -> String -> IO ()
compileFile inputFile outputFile = do
    input <- readFile inputFile
    let res = do 
            toks <- tokenise input
            (fs, _) <- parseFunctions toks
            return fs
    let is = showInstructions <$> compileFunctions <$> res
    let output = eitherToString is
    
    writeFile outputFile output

eitherToString :: Either Error String -> String
eitherToString (Left err) = show err
eitherToString (Right x) = x
