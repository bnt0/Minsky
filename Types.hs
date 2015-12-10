module Types where
import Control.Monad.State.Lazy

type Program = [Instruction]
data Instruction = Inc Register Label | Dec Register Label Label | Halt
    deriving Eq
data Label = Lab Int
    deriving Eq
data Register = Reg Int
    deriving Eq

instance Show Instruction where
    show (Inc r l)     = show r ++ "+ -> " ++ show l
    show (Dec r l1 l2) = show r ++ "+ -> " ++ show l1 ++ ", " ++ show l2
    show Halt          = "HALT"
instance Show Register where
    show (Reg i) = "R" ++ show i
instance Show Label where
    show (Lab i) = "L" ++ show i

