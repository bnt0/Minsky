module Translator where
import Types

import qualified Data.Map as M

-- TODO rewrite using Data.Bits

eval :: Pair -> Int
eval (Num i) = i
eval (DPair p1 p2) = 2^x * (2*y + 1)
    where
        x = eval p1
        y = eval p2
eval (SPair p1 p2) = 2^x * (2*y + 1) - 1
    where
        x = eval p1
        y = eval p2

-------------------------------------------------------------------------------

intToInstr :: Int -> Instruction
intToInstr x 
    | x == 0    = Halt
    | even y    = Inc (Reg regNum) (Lab z)
    | otherwise = Dec (Reg regNum) (Lab t) (Lab f)
        where
            DPair (Num y) (Num z) = intToDPair x
            regNum = (y `div` 2)
            SPair (Num t) (Num f) = intToSPair z

instrToInt :: Instruction -> Int
instrToInt Halt 
    = 0
instrToInt (Inc (Reg regNum) (Lab l))
    = pairToInt (DPair (Num $ 2 * regNum) (Num l))
instrToInt (Dec (Reg regNum) (Lab l1) (Lab l2))
    = pairToInt (DPair (Num $ 2 * regNum + 1) (SPair (Num l1) (Num l2)))


intToDPair :: Int -> Pair
intToDPair i
    = DPair (Num x) (Num y)
        where
            x = greatestTwosPowerDivisor i 
            y = ((i `div` (2 ^ x)) - 1) `div` 2

intToSPair :: Int -> Pair
intToSPair i
    = SPair (Num x) (Num y)
        where
            x = greatestTwosPowerDivisor (i + 1)
            y = (((i + 1) `div` (2 ^ x)) - 1) `div` 2


pairToInt :: Pair -> Int
pairToInt (Num n) = n
pairToInt (DPair x y)
    = 2^x' * (y' * 2 + 1)
        where
            x' = pairToInt x
            y' = pairToInt y
pairToInt (SPair x y)
    = 2^x' * (y' * 2 + 1) - 1
        where
            x' = pairToInt x
            y' = pairToInt y


-- Pre: i > 0
greatestTwosPowerDivisor :: Int -> Int
greatestTwosPowerDivisor i
    | even i    = 1 + (greatestTwosPowerDivisor $ i `div` 2)
    | otherwise = 0

-------------------------------------------------------------------------------

intListToInt :: [Int] -> Int
intListToInt lst = foldr encodeElem 0 lst
    where
        encodeElem x y = 2^x * (2 * y + 1)

intToIntList :: Int -> [Int]
intToIntList i
    | i == 0    = []
    | otherwise = x : intToIntList xs
        where
            x  = greatestTwosPowerDivisor i
            xs = (i `div` (2 ^ x) - 1) `div` 2
        
-------------------------------------------------------------------------------

codeToProgram :: Int -> Program
codeToProgram c
    = M.fromAscList $ zip labelList instrList
        where
            instrList = map intToInstr $ intToIntList c 
            labelList = map (\ x -> (Lab x)) [0..]

programToCode :: Program -> Int
programToCode p
    = intListToInt $ map (instrToInt . snd) (M.toAscList p) 
