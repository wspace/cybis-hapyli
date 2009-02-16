module Main where

import Ast
import FileLoader (loadProgram, loadAssembly)
import Preprocessor (preprocess)
import Validator (getValidationErrors)
import Compiler (compile)
import Assembler (assemble)
import System (getArgs)
import Data.List (intersperse)

main :: IO ()
main = do
    args <- getArgs
    case args of 
        inputFile:outputFile:[] -> compileAndWrite inputFile outputFile
        _ -> usage
             
usage :: IO ()
usage = do
    putStrLn "***** HaPyLi -> WSpace Compiler *****"
    putStrLn "*****     By Kevin Gundlach     *****"
    putStrLn ""
    putStrLn "Usage: hwc <input_file> <output_file>"
        
compileAndWrite :: String -> String -> IO ()
compileAndWrite inputFile outputFile = do
    rawPrg <- loadProgram inputFile
    let procPrg = preprocess rawPrg
    let errorList = getValidationErrors procPrg
    case errorList of
        [] -> do let asmCode = compile procPrg
                 let wspaceCode = assemble asmCode
                 writeFile outputFile wspaceCode
                 putStrLn "Success!"
        errors -> do putStrLn "Compilation failed with the following errors:\n"
                     putStrLn $ concat $ "\n" `intersperse` errors
