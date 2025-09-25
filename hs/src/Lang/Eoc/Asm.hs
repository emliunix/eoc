module Lang.Eoc.Asm where

import Control.Monad.State (State, get, put, runState)
import Control.Exception (throw)

import Data.Map (Map)
import qualified Data.Map as Map

import Lang.Eoc.CVar
import Lang.Eoc.Types (Var, MyException(..), PrimOp(..))

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
  deriving (Show)

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
  | INeg Arg Arg
  | IMv Arg Arg
  | ILabel String Instr

instance Show Instr where
  show (IAdd d s0 s1) = "    add     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (INeg d s)     = "    neg     " ++ show d ++ ", " ++ show s
  show (IMv d s)      = "    mv      " ++ show d ++ ", " ++ show s
  show (ILabel lbl instr) = lbl ++ ":\n" ++ show instr

-- pass: select-instructions

selAtm :: CAtm -> Arg
selAtm (CInt i) = ArgImm i
selAtm (CVar v) = ArgVar v

selStmt :: Arg -> CExp -> [Instr]
selStmt dst expr =
    case expr of
      CPrim PrimNeg [a] -> [INeg dst (selAtm a)]
      CPrim PrimPlus [a, b] -> [IAdd dst (selAtm a) (selAtm b)]
      CAtm a -> [IMv dst (selAtm a)]
      _ -> throw $ MyException $ "Unexpected expr, got " ++ show expr

selTail :: Tail -> [Instr]
selTail (Seq (Assign v expr) tail) = selStmt (ArgVar v) expr ++ selTail tail
selTail (Return expr) = selStmt (ArgReg A0) expr

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

lookupHome :: Var -> State Homes Int
lookupHome v = do
  Homes _ m <- get
  return $ case Map.lookup v m of
    Just h -> h
    Nothing -> throw $ MyException $ "variable not found: " ++ v

auxArg :: Map Var CType -> Arg -> State Homes Arg
auxArg locals (ArgVar v) = do
  h <- allocHome v (case Map.lookup v locals of
    Just ty -> ty
    Nothing -> throw $ MyException $ "variable not found: " ++ v)
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
  in
    AsmProgram (AsmInfo Nothing (Just stackSpace)) instrs'
assignHomes _ = throw $ MyException "required localsTypes not provided"
