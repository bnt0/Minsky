module Parser where
import Types

import Text.Parsec 
import Data.Map (fromList)

runParser :: String -> SourceName -> Either ParseError Program
runParser fp s = parse program fp s

program :: Parsec String () Program
program = do
    spaces
    prog <- many (labelledInstr <* newline)
    spaces
    eof
    return $ fromList prog
        where
            labelledInstr = do
                l <- instLabel
                spaces ; char ':' ; spaces
                i <- (haltInstruction <|> regInstruction) 
                return (l, i)

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
