module Lang.Eoc.Asm.Types where
import Control.Monad.State (State, get, put, runState, evalState)
import Control.Exception (throw)

import Data.Map (Map)
import qualified Data.Map as Map

import Lang.Eoc.CVar
import Lang.Eoc.Types (Var, MyException(..), PrimOp(..))
import Control.Exception.Base (patError)

data AsmInfo = AsmInfo
  { localsTypes :: Maybe (Map Var CType)
  , stackSpace :: Maybe Int
  } deriving (Show)

data Asm = AsmProgram AsmInfo [Instr]

instance Show Asm where
  show (AsmProgram (AsmInfo locals stack) instrs) =
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

data Arg
  = ArgVar Var
  | ArgReg Reg
  | ArgImm Int
  | ArgImmReg Int Reg

instance Show Arg where
  show (ArgVar v) = show v
  show (ArgReg r) = show r
  show (ArgImm i) =  show i
  show (ArgImmReg i r) = show i ++ "(" ++ show r ++ ")"

data Instr
  = IAdd Arg Arg Arg
  | IAddi Arg Arg Arg
  | INeg Arg Arg
  | IMv Arg Arg -- pseudo-instruction, translate to Ld/St afterwards
  | ILd Arg Arg
  | ILi Arg Arg
  | ISt Arg Arg
  | ILabel String Instr

instance Show Instr where
  show (IAdd d s0 s1)     = "    add     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (IAddi d s0 s1)    = "    addi    " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (INeg d s)         = "    neg     " ++ show d ++ ", " ++ show s
  show (IMv d s)          = "    mv      " ++ show d ++ ", " ++ show s
  show (ILd d s)          = "    ld      " ++ show d ++ ", " ++ show s
  show (ILi d s)          = "    li      " ++ show d ++ ", " ++ show s
  show (ISt s d)          = "    st      " ++ show s ++ ", " ++ show d
  show (ILabel lbl instr) = lbl ++ ":\n" ++
                            show instr
