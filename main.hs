module Main where

import FileLoader
import Preprocessor

main :: IO ()
main = do
    p1 <- loadProgram "test.txt"
    let p2 = preprocess p1
    putStrLn (show p2)
    