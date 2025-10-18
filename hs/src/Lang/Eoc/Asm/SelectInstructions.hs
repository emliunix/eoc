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

selTail :: String -> Tail -> [Instr]
selTail conclLbl tail =
  case tail of
    (Seq stmt tail) -> selStmt stmt ++ selTail conclLbl tail
    (Goto lbl) -> [Ibranch lbl]
    (Return exp) -> selExp (ArgReg A0) exp ++ [Ibranch conclLbl]
    (IfStmt cmp a b (Goto lbThn) (Goto lbEls))
      | Just res <- evalCmp cmp a b -> if res then [Ibranch lbThn] else [Ibranch lbEls]
      -- branch to else, fall through to then, following common CPU conventions
      | otherwise -> [nCondBr cmp a b lbEls, Ibranch lbThn]
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
    t -> throw $ MyException $ "invalid tail: " ++ show t

selStmt :: Stmt -> [Instr]
selStmt (Assign var expr) = selExp (ArgVar var) expr
selStmt (StmtCall fn args) =
  zipWith Pmv (map ArgReg argRegs) (map selAtm args) ++
  [ IindirectCall (selAtm fn) (length args)
  ]
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
selExp dst (CFunRef ref) = [Ila dst (ArgGlobal (sanitifyName ref))]
selExp dst (CCall fn args) =
  zipWith Pmv (map ArgReg argRegs) (map selAtm args) ++
  [ IindirectCall (selAtm fn) (length args)
  , Pmv dst (ArgReg A0)
  ]

selArgs :: [String] -> [Instr]
selArgs xs =
   zipWith mkMv xs argRegs
  where
    mkMv var reg = Pmv (ArgVar var) (ArgReg reg)

sanitifyName :: String -> String
sanitifyName = concatMap replaceChar
  where
    replaceChar c
      | c `elem` allowedChars = [c]
      | otherwise = "_"
    allowedChars = ['a'..'z'] ++ ['A'..'Z']

selectInstructions :: CDefs -> PassM AsmDefs
selectInstructions (CDefsProgram info defs) = AsmDefsProgram emptyAsmInfo <$> traverse goDef defs
  where
    goDef (CDef defInfo name args rety blks) = do
      conclLbl <- ("conclusion" ++) . show <$> freshBlockId
      let asmDefInfo = emptyAsmDefInfo (startBlockLabel defInfo) conclLbl
      let instrs = concatMap (mkBlk conclLbl) blks
      return $ AsmDef asmDefInfo (sanitifyName name) instrs
      where
        argsInstrs = selArgs (map fst args)
        startLbl = startBlockLabel defInfo
        mkBlk conclLbl (lbl, tail) =
          let instrs = selTail conclLbl tail
              instrs' = if lbl == startLbl
                then argsInstrs ++ instrs
                else instrs
          in case instrs' of
               i:is -> Ilabel lbl i : is
               [] -> [Ilabel lbl noop]
