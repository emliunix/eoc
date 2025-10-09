module Lang.Eoc.Asm.Types where

import Control.Monad.State (State, get, put, runState, evalState)
import Control.Exception (throw)

import Data.Function ((&))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.CVar
import Lang.Eoc.Types (Var, MyException(..), PrimOp(..))
import Control.Exception.Base (patError)

data AsmInfo = AsmInfo
  { localsTypes :: Maybe (Map Var CType)
  , stackSpace :: Maybe Int
  , livesMap :: Maybe (Map String (Set Arg)) -- ^ label to live variables before that point
  } deriving (Show)

data Asm = AsmProgram AsmInfo [Instr]

instance Show Asm where
  show (AsmProgram (AsmInfo locals stack lives) instrs) =
    ".localsTypes = " ++ show locals ++ "\n" ++
    ".stackSpace = " ++ show stack ++ "\n" ++
    concatMap (\i -> show i ++ "\n") instrs

data Reg
  = X0 | RA | SP | GP | TP
  | T0 | T1 | T2
  | S0 | S1
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
  | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
  | T3 | T4 | T5 | T6
  deriving (Eq, Ord)

instance Show Reg where
  show X0  = "x0"
  show RA  = "ra"
  show SP  = "sp"
  show GP  = "gp"
  show TP  = "tp"
  show T0  = "t0"
  show T1  = "t1"
  show T2  = "t2"
  show S0  = "s0"
  show S1  = "s1"
  show A0  = "a0"
  show A1  = "a1"
  show A2  = "a2"
  show A3  = "a3"
  show A4  = "a4"
  show A5  = "a5"
  show A6  = "a6"
  show A7  = "a7"
  show S2  = "s2"
  show S3  = "s3"
  show S4  = "s4"
  show S5  = "s5"
  show S6  = "s6"
  show S7  = "s7"
  show S8  = "s8"
  show S9  = "s9"
  show S10 = "s10"
  show S11 = "s11"
  show T3  = "t3"
  show T4  = "t4"
  show T5  = "t5"
  show T6  = "t6"

callerSavedRegs :: [Reg]
callerSavedRegs =
  [ A0, A1, A2, A3, A4, A5, A6, A7
  , T0, T1, T2, T3, T4, T5, T6
  ]

data Arg
  = ArgVar Var
  | ArgReg Reg
  | ArgImm Int
  | ArgMemRef Int Reg
  deriving (Eq, Ord)

instance Show Arg where
  show (ArgVar v) = show v
  show (ArgReg r) = show r
  show (ArgImm i) =  show i
  show (ArgMemRef i r) = show i ++ "(" ++ show r ++ ")"

data BranchCond
  = Beq Arg Arg | Beqz Arg
  | Bne Arg Arg | Bnez Arg
  | Blt Arg Arg | Bltz Arg
  | Bgt Arg Arg | Bgtz Arg
  | Ble Arg Arg | Blez Arg
  | Bge Arg Arg | Bgez Arg
  deriving (Eq)

data Instr
  = IAdd Arg Arg Arg
  | IAddi Arg Arg Arg
  | INeg Arg Arg
  | IMv Arg Arg -- pseudo-instruction, translate to Ld/St afterwards
  | ILd Arg Arg
  | ILi Arg Arg
  | ISt Arg Arg
  | IBranch String
  | ICondBranch BranchCond String
  | ICall String
  | ILabel String Instr
  deriving (Eq)

instance Show Instr where
  show (IAdd d s0 s1)         = "    add     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (IAddi d s0 s1)        = "    addi    " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (INeg d s)             = "    neg     " ++ show d ++ ", " ++ show s
  show (IMv d s)              = "    mv      " ++ show d ++ ", " ++ show s
  show (ILd d s)              = "    ld      " ++ show d ++ ", " ++ show s
  show (ILi d s)              = "    li      " ++ show d ++ ", " ++ show s
  show (ISt s d)              = "    st      " ++ show s ++ ", " ++ show d
  show (IBranch lbl)          = "    j       " ++ lbl
  show (ICondBranch cond lbl) = "    " ++ show cond ++ ", " ++ lbl
  show (ICall func)           = "    call    " ++ func
  show (ILabel lbl instr)     = lbl ++ ":\n" ++
                                show instr

instance Show BranchCond where
  show (Beq a b)  = "beq     " ++ show a ++ ", " ++ show b
  show (Beqz a)   = "beqz    " ++ show a
  show (Bne a b)  = "bne     " ++ show a ++ ", " ++ show b
  show (Bnez a)   = "bnez    " ++ show a
  show (Blt a b)  = "blt     " ++ show a ++ ", " ++ show b
  show (Bltz a)   = "bltz    " ++ show a
  show (Bgt a b)  = "bgt     " ++ show a ++ ", " ++ show b
  show (Bgtz a)   = "bgtz    " ++ show a
  show (Ble a b)  = "ble     " ++ show a ++ ", " ++ show b
  show (Blez a)   = "blez    " ++ show a
  show (Bge a b)  = "bge     " ++ show a ++ ", " ++ show b
  show (Bgez a)   = "bgez    " ++ show a

-- | Immediate is not considered a location
locs' :: [Arg] -> Set Arg
locs' xs =
  go xs Set.empty
  where
    go [] s = s
    go (i:is) s =
      case i of
        (ArgVar _) -> go is (Set.insert i s)
        (ArgReg _) -> go is (Set.insert i s)
        (ArgMemRef _ _) -> go is (Set.insert i s)
        _ -> go is s

readLocs :: Instr -> Set Arg
readLocs (IAdd _ s1 s2) = locs' [s1, s2]
readLocs (IAddi _ s1 _) = locs' [s1]
readLocs (INeg _ s) = locs' [s]
readLocs (IMv _ s) = locs' [s]
readLocs (ILd _ s) = locs' [s]
readLocs (ILi _ _) = Set.empty
readLocs (ISt s _) = locs' [s]
readLocs (IBranch _) = Set.empty
readLocs (ICondBranch _ _) = Set.empty
readLocs (ICall _) = Set.empty -- TODO: A0-A7, depends on specifc function
readLocs (ILabel _ instr) = readLocs instr

writeLocs :: Instr -> Set Arg
writeLocs (IAdd d _ _) = locs' [d]
writeLocs (IAddi d _ _) = locs' [d]
writeLocs (INeg d _) = locs' [d]
writeLocs (IMv d _) = locs' [d]
writeLocs (ILd d _) = locs' [d]
writeLocs (ILi d _) = locs' [d]
writeLocs (ISt d _) = locs' [d]
writeLocs (IBranch _) = Set.empty
writeLocs (ICondBranch _ _) = Set.empty
writeLocs (ICall _) = locs' [ArgReg A0]
writeLocs (ILabel _ instr) = writeLocs instr

liveBefore :: Instr -> Set Arg -> Set Arg
liveBefore instr s = s & flip Set.difference (writeLocs instr) & Set.union (readLocs instr)

splitBlocks :: [Instr] -> [(String, [Instr])]
splitBlocks (i@(ILabel lbl _):is) =
  let (l', is', bs) = foldl go (lbl, [i], []) is in
    reverse $ (l', reverse is') : bs
  where
    go (lbl, iAcc, bAcc) i@(ILabel lbl' _) =
      let bAcc' = (lbl, reverse iAcc) : bAcc
      in (lbl', [i], bAcc')
    go (lbl, iAcc, bAcc) i = (lbl, i:iAcc, bAcc)
splitBlocks _ = throw $ MyException "instruction sequence must start with a label"
