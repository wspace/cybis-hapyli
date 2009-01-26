module Main where

import FileLoader
import Preprocessor
import Validator 

main :: IO ()
main = do
    p1 <- loadProgram "test.txt"
    let p2 = preprocess p1
    let errors = getValidationErrors p2
    mapM_ putStrLn errors
    putStrLn "=============="
    putStrLn (show p2)
    