module Lang.Eoc.Asm where

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
  show (ILabel lbl instr) = lbl ++ ":\n" ++ show instr

-- pass: select-instructions

selAtm :: CAtm -> Arg
selAtm (CInt i) = ArgImm i
selAtm (CVar v) = ArgVar v

selStmtAssign :: Arg -> CExp -> [Instr]
selStmtAssign dst expr =
    case expr of
      CPrim PrimNeg [a] -> [INeg dst (selAtm a)]
      CPrim PrimPlus [a, b] -> [IAdd dst (selAtm a) (selAtm b)]
      CAtm a -> [IMv dst (selAtm a)]
      _ -> throw $ MyException $ "Unexpected expr, got " ++ show expr

selTail :: Tail -> [Instr]
selTail (Seq (Assign v expr) tail) = selStmtAssign (ArgVar v) expr ++ selTail tail
selTail (Return expr) = selStmtAssign (ArgReg A0) expr

selectInstructions :: CVar -> Asm
selectInstructions cprg@(CProgram _ [(_, tail)]) =
  AsmProgram (AsmInfo (Just localsTypes) Nothing) (selTail tail)
  where
    localsTypes = typeCheckCVar cprg
selectInstructions _ = throw $ MyException "Multi labeled CVar program not supported yet"

-- pass: assign-homes

data Homes = Homes Int (Map Var Int)

allocHome :: Var -> CType -> State Homes Int
allocHome v _ty = do
  Homes s m <- get
  let m' = Map.insert v s m
  -- TODO: it should be sizeof(ty), but it's all ints for now
  put $ Homes (s+4) m'
  return s

lookupHome :: Var -> State Homes (Maybe Int)
lookupHome v = do
  Homes _ m <- get
  return $ Map.lookup v m

auxArg :: Map Var CType -> Arg -> State Homes Arg
auxArg locals (ArgVar v) = do
  h <- lookupHome v
  case h of
    Just h -> pure $ ArgImmReg h S0
    Nothing ->
      let cty = case Map.lookup v locals of
            Just ty -> ty
            Nothing -> throw $ MyException $ "variable not found: " ++ v
      in do
        h <- allocHome v cty
        return $ ArgImmReg h S0
auxArg _ arg = return arg

auxInstr :: Map Var CType -> Instr -> State Homes Instr
auxInstr locals instr =
  let auxArg' = auxArg locals in
    case instr of
      (IAdd d s0 s1) -> do
        d <- auxArg' d
        s0 <- auxArg' s0
        s1 <- auxArg' s1
        return $ IAdd d s0 s1
      (INeg d s) -> do
        d <- auxArg' d
        s <- auxArg' s
        return $ INeg d s
      (IMv d s) -> do
        d <- auxArg' d
        s <- auxArg' s
        return $ IMv d s
      (ILabel lbl instr) ->
        ILabel lbl <$> auxInstr locals instr

assignHomes :: Asm -> Asm
assignHomes (AsmProgram (AsmInfo (Just locals) _) instrs) =
  let initHomes = Homes 16 Map.empty -- stack frame always keeps lr fp
      (instrs', Homes stackSpace _) = runState (traverse (auxInstr locals) instrs) initHomes
      stackSpace' = Just $ ((stackSpace + 15) `div` 16) * 16 -- align to 16 bytes
  in
    AsmProgram (AsmInfo Nothing stackSpace') instrs'
assignHomes _ = throw $ MyException "required localsTypes not provided"

-- | ensure source is loaded into a register if it's not
patchS :: Reg -> Arg -> (Arg -> [Instr]) -> [Instr]
patchS _ s@(ArgReg _)      f = f s
patchS r s@(ArgImm _)      f = let ar = ArgReg r
                               in ILi ar s : f ar
patchS r s@(ArgImmReg _ _) f = let ar = ArgReg r
                               in ILd ar s : f ar
patchS _ s                 _ = throw $ MyException $ "invalid source: " ++ show s

-- | ensure destination is a register, if not, use tmp register then store afterwards
patchD :: Reg -> Arg -> (Arg -> [Instr]) -> [Instr]
patchD _ d@(ArgReg _)      f = f d
patchD _   (ArgImm _)      _ = throw $ MyException "destination cannot be immediate"
patchD r d@(ArgImmReg _ _) f = let dr = ArgReg r
                               in f dr ++ [ISt dr d]
patchD _ d                 _ = throw $ MyException $ "invalid destination: " ++ show d

patchInstructions :: Asm -> Asm
patchInstructions (AsmProgram info instrs) =
  AsmProgram info (concatMap patch instrs)
  where
    -- add
    patch (IAdd d s0 si@(ArgImm _)) =
      patchS T1 s0
      (\s0' ->
         patchD T0 d
         (\d' -> [IAddi d' s0' si]))
    patch (IAdd d s0 s1) =
      patchS T1 s0
      (\s0' ->
         patchS T2 s1
         (\s1' ->
            patchD T0 d
            (\d' -> [IAdd d' s0' s1'])))
    -- mv
    patch (IMv d@(ArgReg _) s@(ArgReg _)) = [IAddi d s (ArgImm 0)] -- mv rd, rs  ==> addi rd, rs, 0
    patch (IMv d@(ArgImmReg _ _) s@(ArgReg _)) = [ISt d s] -- mv rd, rs  ==> addi rd, rs, 0
    patch (IMv d s) =
      patchS T0 s
      (\_ ->
         patchD T0 d
         (const []))
    -- neg
    patch (INeg d s) =
      patchS T1 s
      (\s' ->
         patchD T0 d
         (\d' -> [INeg d' s']))
    -- label
    patch (ILabel lbl instr) =
      let i:is = patch instr in -- patch only expands
        ILabel lbl i : is
    patch i = [i]
