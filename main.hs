module Main where

import FileLoader

main :: IO ()
main = do
    m <- loadModule "test.txt"
    putStrLn (show m)
    