module Lang.Eoc.Asm.PatchInstructions where

import Control.Exception (throw)
import Control.Monad.State (State, get, put, runState, MonadState, modify)

import Lang.Eoc.Types (PassM, MyException(..))
import Lang.Eoc.Asm.Types

data PatchState = PatchState
 { remainRegs :: [Reg]
 , loads :: [Instr] -- (d, s)
 , stores :: [Instr] -- (s, d)
 }

newtype PatchM a = PatchM { runPatchM :: State PatchState a }
  deriving (Functor, Applicative, Monad, MonadState PatchState)

allocReg :: PatchM Reg
allocReg = do
  st <- get
  case remainRegs st of
    (r:rs) -> do
      put $ st { remainRegs = rs }
      return r
    [] -> throw $ MyException "ran out of registers in PatchM"

-- | if source arg is not a register, load it into a register
patchS :: Arg -> PatchM Arg
patchS s =
  case s of
    ArgReg _ -> return s
    ArgImm 0 -> return (ArgReg X0) -- optimize load of 0
    ArgImm _ -> do
      r <- allocReg
      modify $ \st -> st { loads = loads st ++ [Ili (ArgReg r) s]}
      return (ArgReg r)
    ArgMemRef _ _ -> do
      r' <- allocReg
      modify $ \st -> st { loads = loads st ++ [Ild (ArgReg r') s] }
      return (ArgReg r')
    _ -> throw $ MyException $ "unsupported source arg in patchS: " ++ show s

-- | if dest arg is MemRef, allocate a register and store to memory after patch
patchD :: Arg -> PatchM Arg
patchD d =
  case d of
    ArgReg _ -> return d
    ArgMemRef _ _ -> do
      r' <- allocReg
      modify $ \st -> st { stores = stores st ++ [Ist (ArgReg r') d] }
      return (ArgReg r')
    _ -> throw $ MyException $ "unsupported dest arg in patchD: " ++ show d

patchDSS :: Arg -> Arg -> Arg -> PatchM (Arg, Arg, Arg)
patchDSS d s0 s1 = do
  s0' <- patchS s0
  s1' <- patchS s1
  d' <- patchD d
  return (d', s0', s1')

patchDS :: Arg -> Arg -> PatchM (Arg, Arg)
patchDS d s = do
  s' <- patchS s
  d' <- patchD d
  return (d', s')

patchSS :: Arg -> Arg -> PatchM (Arg, Arg)
patchSS s0 s1 = do
  s0' <- patchS s0
  s1' <- patchS s1
  return (s0', s1')

runPatch :: PatchM [Instr] -> [Instr]
runPatch (PatchM p) =
  let (emits, st) = runState p (PatchState [T0, T1, T2] [] [])
  in loads st ++ emits ++ stores st

class ToInstrs a where
  toInstrs :: a -> [Instr]

instance ToInstrs Instr where
  toInstrs i = [i]

instance ToInstrs [Instr] where
  toInstrs = id

-- | Handle the pattern of the instructions with two commutative source operands
-- and possibly an immediate source operand.
-- eg. add addi, xor xori, and andi, or ori
commBinImm ::
  forall a b. (ToInstrs a, ToInstrs b) =>
  -- | d s s case
  (Arg -> Arg -> Arg -> a) ->
  -- | d s imm case
  (Arg -> Arg -> Arg -> b) ->
  Arg -> Arg -> Arg -> [Instr]
commBinImm fReg fImm d s1 s2 =
  runPatch $ go d s1 s2
  where
    go d s0 (ArgImm 0) = do
      (d', s0') <- patchDS d s0
      return $ toInstrs $ fReg d' (ArgReg X0) s0'
    go d s0 s1@(ArgImm _) = do
      (d', s0') <- patchDS d s0
      return $ toInstrs $ fImm d' s0' s1
    go d s0@(ArgImm _) s1 = go d s1 s0
    go d s1 s2 = do
      (d', s1', s2') <- patchDSS d s1 s2
      return $ toInstrs $ fReg d' s1' s2'

patchCond :: BranchCond -> PatchM BranchCond
patchCond cond = case cond of
  Beq (ArgImm 0) s1 -> patchCond (Beqz s1)
  Beq s0 (ArgImm 0) -> patchCond (Beqz s0)
  Beq s0 s1 -> do
    (s0', s1') <- patchSS s0 s1
    return $ Beq s0' s1'
  Beqz s -> do
    s' <- patchS s
    return $ Beqz s'
  Bne (ArgImm 0) s1 -> patchCond (Bnez s1)
  Bne s0 (ArgImm 0) -> patchCond (Bnez s0)
  Bne s0 s1 -> do
    (s0', s1') <- patchSS s0 s1
    return $ Bne s0' s1'
  Bnez s -> do
    s' <- patchS s
    return $ Bnez s'
  Blt s0 s1 -> do
    (s0', s1') <- patchSS s0 s1
    return $ Blt s0' s1'
  Bge s0 s1 -> do
    (s0', s1') <- patchSS s0 s1
    return $ Bge s0' s1'

patchInstrs :: [Instr] -> [Instr]
patchInstrs [] = []
patchInstrs (Ilabel l i : is) = case patchInstrs (i:is) of
  i:is -> Ilabel l i:is
  [] -> [Ilabel l noop] -- NOP
patchInstrs (i:is) =
  case i of
    Pmv a b | a == b -> cont  -- remove redundant move
    -- commutative binary op with immediate alt
    Pmv d@(ArgReg _) s@(ArgImm _) -> Ili d s : cont
    Pmv d@(ArgReg _) s@(ArgMemRef _ _) -> Ild s d : cont
    Pmv d@(ArgReg _) s@(ArgReg _) -> Iaddi d s (ArgImm 0) : cont
    Pmv d@(ArgMemRef _ _) s -> runPatch st ++ cont
      where
        st = do
          s' <- patchS s
          return [Ist s' d]
    Iadd d s0 s1 -> commBinImm Iadd Iaddi d s0 s1 ++ cont
    Iand d s0 s1 -> commBinImm Iand Iandi d s0 s1 ++ cont
    Ior d s0 s1 -> commBinImm Ior Iori d s0 s1 ++ cont
    Ixor d s0 s1 -> commBinImm Ixor Ixori d s0 s1 ++ cont
    -- ensure load-store
    Ineg d s -> runPatch neg ++ cont
      where
        neg = do
          (d', s') <- patchDS d s
          return [Ineg d' s']
    Isub d s0 s1 -> runPatch sub ++ cont
      where
        sub = do
          (d', s0', s1') <- patchDSS d s0 s1
          return [Isub d' s0' s1']
    -- pseudo set cmp instructions
    Pnot d s -> runPatch not' ++ cont
      where
        not' = do
          (d', s') <- patchDS d s
          return [Ixori d' s' (ArgImm 1)]
    Pseq d s0 s1 -> commBinImm seq seqi d s0 s1 ++ cont
      where
        seq d s0 s1 = [Ixor d s0 s1, Isltiu d d (ArgImm 1)] -- d == 0
        seqi d s0 i = [Ixori d s0 i, Isltiu d d (ArgImm 1)]
    Psne d s0 s1 -> commBinImm sne snei d s0 s1 ++ cont
      where
        sne d s0 s1 = [Ixor d s0 s1, Isltu d (ArgReg X0) d] -- d > 0
        snei d s0 i = [Ixori d s0 i, Isltu d (ArgReg X0) d]
    Psle d s0 s1 -> runPatch sle ++ cont
      -- a <= b <=> not (b < a)
      where
        sle = case s0 of
          ArgImm i -> do
            s1' <- patchS s1
            return [Islti d s1' (ArgImm i), Ixori d d (ArgImm 1)]
          _ -> do
            (s0', s1') <- patchSS s0 s1
            return [Islt d s1' s0', Ixori d d (ArgImm 1)]
    IcondBranch cond lbl ->
      condBranch ++ cont
      where
        condBranch = runPatch $ do
          cond' <- patchCond cond
          return [IcondBranch cond' lbl]
    Ibranch lbl
      | (Ilabel l' _ : _) <- cont, lbl == l' -> cont
    _ -> i : cont
  where
    cont = patchInstrs is

patchInstructions :: AsmDefs -> PassM AsmDefs
patchInstructions (AsmDefsProgram info defs) = AsmDefsProgram info <$> traverse goDef defs
  where
    goDef (AsmDef info name instrs) =
      return $ AsmDef info name (patchInstrs instrs)
