module Parser where
import Types

import Text.Parsec 
import Data.Map (fromList)

program :: Parsec String () Program
program = do
    spaces
    prog <- many $ (,) 
                <$> instLabel <*> (haltInstruction <|> regInstruction)
    return $ fromList prog

regInstruction :: Parsec String () Instruction
regInstruction = do
    r <- register
    t <- oneOf "+-"
    spaces
    string "->"
    spaces
    case t of
        '+' -> do
            l <- instLabel
            return (Inc r l)
        '-' -> do
            l1 <- instLabel
            spaces
            char ','
            spaces
            l2 <- instLabel
            return (Dec r l1 l2)

haltInstruction :: Parsec String () Instruction
haltInstruction = (\_ -> Halt) <$> string "HALT"

instLabel :: Parsec String () Label
instLabel = do
    char 'L'
    num <- many1 digit
    return (Lab $ read num)
    
register :: Parsec String () Register
register = do
    char 'R'
    n <- many1 digit
    return (Reg $ read n)
