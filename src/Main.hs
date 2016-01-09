module Main where
import Parser

import System.Environment

main :: IO ()
main = do
    (file:_) <- getArgs
    src <- readFile file
    print $ runParser src
