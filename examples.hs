module Examples where
import Types
import Executor

import qualified Data.Map as M

-- L0 : R1− -> L1, L7
-- L1 : R0+ -> L2
-- L2 : R2− -> L3, L5
-- L3 : R3+ -> L4
-- L4 : R0+ -> L1
-- L5 : R2+ -> L6
-- L6 : R3− -> L5, L0
-- L7 : HALT

prog1 :: Program
prog1 = M.fromList 
    [ (Lab 0, Dec (Reg 1) (Lab 1) (Lab 7))
    , (Lab 1, Inc (Reg 0) (Lab 2))
    , (Lab 2, Dec (Reg 2) (Lab 3) (Lab 5))
    , (Lab 3, Inc (Reg 3) (Lab 4))
    , (Lab 4, Inc (Reg 0) (Lab 1))
    , (Lab 5, Inc (Reg 2) (Lab 6))
    , (Lab 6, Dec (Reg 3) (Lab 5) (Lab 0))
    , (Lab 8, Halt) ]


conf1 :: Configuration
conf1 = ((Lab 0), rvs)
    where
        rvs = M.fromList
            [ ((Reg 0), 0)
            , ((Reg 1), 2)
            , ((Reg 2), 0)
            , ((Reg 3), 0)
            ]
