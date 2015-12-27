module Executor where
import Types 

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M


-- If a referenced register doesn't exist in the configuration, 
-- the instruction is ignored
runInstr :: Instruction -> State Configuration Label
runInstr (Inc r l) = do
    st <- get
    put st { regVals = M.update (\a -> Just $ a + 1) r $ regVals st }
    return l
runInstr (Dec r l1 l2) = do
    rvs <- gets regVals
    let oldVal = fromJust $ M.lookup r rvs
    if (oldVal == 0)
        then return l2
        else do
            st <- get
            put st { regVals = M.update (\a -> Just $ a - 1) r $ regVals st }
            return l1
runInstr Halt = return EndLabel

runLabel :: Label -> Program -> State Configuration Label
runLabel l@(Lab n) p = do
    let i = M.lookup l p
    case i of
        Nothing -> return ErrHalt
        Just i' -> do
                     l' <- runInstr i'
                     return l'
runLabel hlt _ = return hlt
