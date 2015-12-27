module Types where
import qualified Data.Map as M

data Instruction = Inc Register Label | Dec Register Label Label | Halt
    deriving Eq
data Label = Lab Int | EndLabel | ErrHalt 
    deriving (Eq, Ord)
data Register = Reg Int
    deriving (Eq, Ord)
type Program = M.Map Label Instruction

type Configuration = (Label, RegVals)

type RegVals = M.Map Register Int

emptyRegs :: Int -> RegVals
emptyRegs numRegs = M.fromList $ take numRegs [(Reg r, 0) | r <- [0..]]

data Pair = DPair Pair Pair | SPair Pair Pair | Num Int
    deriving (Show, Eq)

instance Show Instruction where
    show (Inc r l)     = show r ++ "+ -> " ++ show l
    show (Dec r l1 l2) = show r ++ "- -> " ++ show l1 ++ ", " ++ show l2
    show Halt          = "HALT"
instance Show Register where
    show (Reg i) = "R" ++ show i
instance Show Label where
    show EndLabel   = "HALT"
    show ErrHalt    = "ERRHALT"
    show (Lab i)    = "L" ++ show i

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

--------------------------------------------------------------------------------

intToInstr :: Int -> Instruction
intToInstr x 
    | x == 0    = Halt
    | even y    = Inc (Reg regNum) (Lab z)
    | otherwise = Dec (Reg regNum) (Lab t) (Lab f)
        where
            DPair (Num y) (Num z) = intToDPair x
            regNum = (y `div` 2)
            SPair (Num t) (Num f) = intToSPair z

intToDPair :: Int -> Pair
intToDPair i
    = DPair (Num x) (Num y)
        where
            x = greatestTwosPowerDivisor i 
            y = ((i `div` (2^x)) - 1) `div` 2

intToSPair :: Int -> Pair
intToSPair i
    = SPair (Num x) (Num y)
        where
            x = greatestTwosPowerDivisor (i + 1)
            y = (((i + 1) `div` (2 ^ x)) - 1) `div` 2

-- Pre: i > 0
greatestTwosPowerDivisor :: Int -> Int
greatestTwosPowerDivisor i
    | even i    = 1 + (greatestTwosPowerDivisor $ i `div` 2)
    | otherwise = 0

--------------------------------------------------------------------------------

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
        
--------------------------------------------------------------------------------

codeToProgram :: Int -> Program
codeToProgram c
    = M.fromAscList $ zip labelList instrList
        where
            instrList = map intToInstr $ intToIntList c 
            labelList = map (\ x -> (Lab x)) [0..]
