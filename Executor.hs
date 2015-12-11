module Executor where
import Data.Maybe
import Types 

entryPoint :: Label
entryPoint = Lab 0

errHalt :: Label
errHalt = (Lab $ -1)

numReg :: Int
numReg = 3

regList :: [Register]
regList = map (\i -> Reg i) [0..numReg - 1]

initConf :: Configuration
initConf = (entryPoint, regVals)
    where
        regVals = zip regList (repeat 0)


--------------------------------------------------------------------------------

execute :: State -> State
execute s@(p, (l, rs))
    | instr == Nothing = (p, (errHalt, rs))
    | otherwise        = (p, (executeInstr (fromJust instr) rs))
    where
        instr = lookup l p

-- TODO
executeInstr :: Instruction -> [(Register, Int)] -> Configuration
executeInstr (Inc r l) rs
    = initConf
executeInstr (Dec r l1 l2) rs
    = initConf
executeInstr Halt _
    = initConf
