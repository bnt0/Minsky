module Executor where
import Types 

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M


-- If a referenced register doesn't exist in the configuration, 
-- the instruction is ignored
runInstr :: Instruction -> State RegVals Label
runInstr (Inc r l) = do
    rvs <- get
    put $ M.update (\a -> Just $ a + 1) r rvs
    return l
runInstr (Dec r l1 l2) = do
    rvs <- get
    let oldVal = fromJust $ M.lookup r rvs
    if (oldVal == 0)
        then return l2
    else do
        put $ M.update (\a -> Just $ a - 1) r rvs
        return l1
runInstr Halt = return EndLabel


runLabel :: Label -> Program -> State RegVals Label
runLabel l@(Lab n) p =
    case M.lookup l p of
        Nothing -> return ErrHalt
        Just i' -> do
            l'  <- runInstr i'
            if (l' == EndLabel) 
                then return l 
            else do
                l'' <- runLabel l' p
                return l''
runLabel ErrHalt _ = return ErrHalt


execute :: Configuration -> Program -> Configuration
execute (l,rvs) p
    = runState (runLabel l p) rvs
