module Main where
import Parser
import Executor

import System.Environment

main :: IO ()
main = do
    (file:_) <- getArgs
    src <- readFile file
    case runParser file src of
        Left error -> 
            print error
        Right program ->
            print program
            -- TODO count number of registers used
            --      count number of labels
            -- ask for starting configuration
            --  can use emptyRegs from Types.hs
            -- execute accordingly
