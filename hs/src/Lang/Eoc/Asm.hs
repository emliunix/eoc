module Lang.Eoc.Asm where

import Control.Exception (throw)

import Lang.Eoc.CVar
import Lang.Eoc.Types (Var, MyException(..), PrimOp(..))

data AsmInfo
  = AsmInfoNone
  | AsmInfoAssigned { }
  deriving (Show)

data Asm = AsmProgram AsmInfo [Instr]

data Reg
  = X0 | SP | RA | GP | TP
  | T0 | T1 | T2
  | S0 | S1
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
  | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
  | T3 | T4 | T5 | T6

data Arg
  = ArgVar Var
  | ArgReg Reg
  | ArgImm Int
  | ArgImmReg Int Reg

data Instr
  = IAdd Arg Arg Arg
  | INeg Arg Arg
  | IMv Arg Arg
  | ILabel String Instr

-- pass: select-instructions

selAtm :: CAtm -> Arg
selAtm (CInt i) = ArgImm i
selAtm (CVar v) = ArgVar v

selStmt :: Arg -> CExp -> [Instr]
selStmt dst expr =
    case expr of
      CPrim PrimNeg [a] -> [INeg dst (selAtm a)]
      CPrim PrimPlus [a, b] -> [IAdd dst (selAtm a) (selAtm b)]
      _ -> throw $ MyException $ "Unexpected expr, got " ++ show expr

selTail :: Tail -> [Instr]
selTail (Seq (Assign v expr) tail) = selStmt (ArgVar v) expr ++ selTail tail
selTail (Return expr) = selStmt (ArgReg A0) expr

selectInstructions :: CVar -> Asm
selectInstructions (CProgram _ [(_, tail)]) = AsmProgram AsmInfoNone (selTail tail)
selectInstructions _ = throw $ MyException "Multi labeled CVar program not supported yet"

-- pass: assign-homes
-- assignHomes :: Asm -> Asm
