module Parser where
import Types

import Text.Parsec as P

parseIncInstruction :: P.Parsec String () Instruction
parseIncInstruction = do
    P.char 'R'
    regNum <- P.many1 P.digit
    iType <- P.char '+'
    P.spaces
    P.string "->"
    P.spaces
    label <- parseLabel
    return (Inc (Reg $ read regNum) label)

parseLabel :: P.Parsec String () Label
parseLabel = do
    P.char 'L'
    num <- P.many1 P.digit
    return (Lab $ read num)
    
