module Lang.Eoc.Asm.SelectInstructions where

import Lang.Eoc.Types
import Lang.Eoc.C.Types
import Lang.Eoc.Asm.Types
import Control.Exception (throw)

isConst :: CAtm -> Bool
isConst (CInt _) = True
isConst (CBool _) = True
isConst CUnit = True
isConst _ = False

evalCmp :: PrimOp -> CAtm -> CAtm -> Maybe Bool
evalCmp op (CInt a) (CInt b) = Just $ case op of
  PrimEq -> a == b
  PrimNe -> a /= b
  PrimGe -> a >= b
  PrimGt -> a > b
  PrimLe -> a <= b
  PrimLt -> a < b
  _ -> throw $ MyException $ "invalid comparison: " ++ show op ++
       " between " ++ show (CInt a) ++ " and " ++ show (CInt b)
evalCmp op (CBool a) (CBool b) = Just $ case op of
  PrimEq -> a == b
  PrimNe -> a /= b
  _ -> throw $ MyException $ "invalid comparison: " ++ show op ++
       " between " ++ show (CBool a) ++ " and " ++ show (CBool b)
evalCmp op a b
  | CUnit == a || CUnit == b = Just $ case op of
      PrimEq -> True
      PrimNe -> False
      _ -> throw $ MyException $ "invalid comparison: " ++ show op ++
           " between " ++ show CUnit ++ " and " ++ show CUnit
evalCmp _ _ _ = Nothing

selTail :: Tail -> [Instr]
selTail (Seq stmt tail) = selStmt stmt ++ selTail tail
selTail (Goto lbl) = [Ibranch lbl]
selTail (Return exp) = selExp (ArgReg A0) exp
selTail (IfStmt cmp a b (Goto lbThn) (Goto lbEls))
  | Just res <- evalCmp cmp a b = if res then [Ibranch lbThn] else [Ibranch lbEls]
  -- branch to else, fall through to then, following common CPU conventions
  | otherwise = [nCondBr cmp a b lbEls, Ibranch lbThn]
  where
    -- eq / ne
    nCondBr PrimEq a b = IcondBranch (Bne (selAtm a) (selAtm b))
    nCondBr PrimNe a b = IcondBranch (Beq (selAtm a) (selAtm b))
    -- cmp
    nCondBr PrimGe a b = IcondBranch (Bge (selAtm b) (selAtm a))
    nCondBr PrimLe a b = IcondBranch (Bge (selAtm a) (selAtm b))
    nCondBr PrimGt a b = IcondBranch (Blt (selAtm a) (selAtm b))
    nCondBr PrimLt a b = IcondBranch (Blt (selAtm b) (selAtm a))
    -- invalid
    nCondBr op a b = throw $ MyException $ "invalid comparison: " ++ show op ++
                      " between " ++ show a ++ " and " ++ show b
selTail t = throw $ MyException $ "invalid tail: " ++ show t

selStmt :: Stmt -> [Instr]
selStmt (Assign var expr) = selExp (ArgVar var) expr
selStmt (StmtPrim op args) = error "not implemented"

selAtm :: CAtm -> Arg
selAtm atm = case atm of
  CInt i -> ArgImm i
  CBool b -> ArgImm (if b then 1 else 0)
  CUnit -> ArgImm 0
  CVar v -> ArgVar v

selExp :: Arg -> CExp -> [Instr]
selExp dst (CAtm atm) = [Pmv dst (selAtm atm)]
selExp dst (CPrim PrimNeg [a]) = [Ineg dst (selAtm a)]
selExp dst (CPrim PrimPlus [a, b]) = [Iadd dst (selAtm a) (selAtm b)]
selExp dst (CPrim PrimSub [a, b]) = [Isub dst (selAtm a) (selAtm b)]
selExp dst (CPrim cmpOp args)
  | isCmpOp cmpOp, [a, b] <- args = case cmpOp of
      PrimEq -> [Pseq dst (selAtm a) (selAtm b)]
      PrimNe -> [Ixor dst (selAtm a) (selAtm b)]
      PrimLt -> [Islt dst (selAtm a) (selAtm b)]
      PrimLe -> [Psle dst (selAtm a) (selAtm b)]
      PrimGt -> [Islt dst (selAtm b) (selAtm a)]
      PrimGe -> [Psle dst (selAtm b) (selAtm a)]
      _ -> throw $ MyException $ "cmpOp not implemented: " ++ show cmpOp
selExp dst (CPrim PrimNot [a]) =
  -- this relies on we gurantee using 1 for True, instead of any non-zero value
  [Ixori dst (selAtm a) (ArgImm 1)]
selExp _ e@(CPrim _ _) =
  throw $ MyException $ "primop not implemented: " ++ show e

selectInstructions :: C -> PassM Asm
selectInstructions (CProgram info blks) = pure $ AsmProgram emptyAsmInfo instrs
  where
    instrs = concatMap mkBlk blks
    mkBlk (lbl, tail) = case selTail tail of
      i:is -> Ilabel lbl i : is
      [] -> [Ilabel lbl noop]
