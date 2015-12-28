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

