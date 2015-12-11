module Types where

data Instruction = Inc Register Label | Dec Register Label Label | Halt
    deriving Eq
data Label = Lab Int
    deriving Eq
data Register = Reg Int
    deriving (Eq, Ord)
type Configuration = (Label, [(Register, Int)])
type Program       = [(Label, Instruction)]
type State         = (Program, Configuration)

instance Show Instruction where
    show (Inc r l)     = show r ++ "+ -> " ++ show l
    show (Dec r l1 l2) = show r ++ "+ -> " ++ show l1 ++ ", " ++ show l2
    show Halt          = "HALT"
instance Show Register where
    show (Reg i) = "R" ++ show i
instance Show Label where
    show (Lab (-1)) = "ERRHALT"
    show (Lab i)    = "L" ++ show i

