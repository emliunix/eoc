{-# LANGUAGE OrPatterns #-}
module Lang.Eoc.Asm.Types where

import Control.Monad.State (State, get, put, runState, evalState)
import Control.Exception (throw)

import Data.Function ((&))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.CVar
import Lang.Eoc.Types (Var, MyException(..))

data AsmInfo = AsmInfo
  { aiLocalsTypes :: Maybe (Map Var CType)
  , aiStackSpace :: Maybe Int
  , aiLivesMap :: Maybe (Map String (Set Arg)) -- ^ label to live variables before that point
  , aiInterferences :: Maybe (Map Arg (Set Arg))
  , aiMoves :: Maybe (Map Arg (Set Arg))
  } deriving (Show)

emptyAsmInfo :: AsmInfo
emptyAsmInfo = AsmInfo Nothing Nothing Nothing Nothing Nothing

data Asm = AsmProgram AsmInfo [Instr]

instance Show Asm where
  show (AsmProgram (AsmInfo locals stack lives inferences moves) instrs) =
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
  | Blt Arg Arg
  | Bge Arg Arg
  deriving (Eq)

data Instr
  = Iadd Arg Arg Arg
  | Iaddi Arg Arg Arg
  | Ineg Arg Arg
  | Isub Arg Arg Arg

  -- | pseudo-instruction, translate to Ld/St afterwards
  | Pmv Arg Arg
  | Ild Arg Arg
  | Ili Arg Arg
  | Ist Arg Arg

  | Iand Arg Arg Arg
  | Iandi Arg Arg Arg
  | Ior Arg Arg Arg
  | Iori Arg Arg Arg
  | Ixor Arg Arg Arg
  | Ixori Arg Arg Arg
  -- | pseudo instruction, translate to xori dst, src, 1
  | Pnot Arg Arg 

  | Islt Arg Arg Arg
  | Islti Arg Arg Arg
  | Isltu Arg Arg Arg
  | Isltiu Arg Arg Arg
  -- | psuedo set cmp instructions
  | Psne Arg Arg Arg
  | Pseq Arg Arg Arg
  | Psle Arg Arg Arg

  | Ibranch String
  | IcondBranch BranchCond String
  | Icall String
  | Ilabel String Instr
  deriving (Eq)

noop :: Instr
noop = Iaddi (ArgReg X0) (ArgReg X0) (ArgImm 0)

instance Show Instr where
  show (Iadd d s0 s1)         = "    add     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Iaddi d s0 s1)        = "    addi    " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Ineg d s)             = "    neg     " ++ show d ++ ", " ++ show s
  show (Isub d s0 s1)         = "    sub     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1

  show (Pmv d s)              = "    mv      " ++ show d ++ ", " ++ show s
  show (Ild d s)              = "    ld      " ++ show d ++ ", " ++ show s
  show (Ili d s)              = "    li      " ++ show d ++ ", " ++ show s
  show (Ist s d)              = "    st      " ++ show s ++ ", " ++ show d

  show (Iand d s0 s1)         = "    and     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Iandi d s0 s1)        = "    andi    " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Ior d s0 s1)          = "    or      " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Iori d s0 s1)         = "    ori     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Ixor d s0 s1)         = "    xor     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Ixori d s0 s1)        = "    xori    " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Pnot d s)             = "    not     " ++ show d ++ ", " ++ show s

  show (Islt d s0 s1)         = "    slt     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Islti d s0 s1)        = "    slti    " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Isltu d s0 s1)        = "    sltu    " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Isltiu d s0 s1)       = "    sltiu   " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1

  show (Psne d s0 s1)         = "    sne     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Pseq d s0 s1)         = "    seq     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1
  show (Psle d s0 s1)         = "    sle     " ++ show d ++ ", " ++ show s0 ++ ", " ++ show s1

  show (Ibranch lbl)          = "    j       " ++ lbl
  show (IcondBranch cond lbl) = "    " ++ show cond ++ ", " ++ lbl
  show (Icall func)           = "    call    " ++ func
  show (Ilabel lbl instr)     = lbl ++ ":\n" ++
                                show instr

instance Show BranchCond where
  show (Beq a b)  = "beq     " ++ show a ++ ", " ++ show b
  show (Beqz a)   = "beqz    " ++ show a
  show (Bne a b)  = "bne     " ++ show a ++ ", " ++ show b
  show (Bnez a)   = "bnez    " ++ show a
  show (Blt a b)  = "blt     " ++ show a ++ ", " ++ show b
  show (Bge a b)  = "bge     " ++ show a ++ ", " ++ show b

-- | Immediate is not considered a location
locs' :: [Arg] -> Set Arg
locs' xs =
  go xs Set.empty
  where
    go [] s = s
    go (i@(ArgVar _; ArgReg _; ArgMemRef _ _):is) s = go is (Set.insert i s)
    go (_:is) s = go is s

readLocs :: Instr -> Set Arg
readLocs (Ilabel _ instr) = readLocs instr
readLocs instr = case instr of
  Iadd   _ s0 s1   -> locs' [s0, s1]
  Iaddi  _ s0 _    -> locs' [s0]
  Ineg   _ s       -> locs' [s]
  Isub   _ s0 s1   -> locs' [s0, s1]
  Pmv    _ s       -> locs' [s]
  Ild    _ s       -> locs' [s]
  Ili    _ _       -> Set.empty
  Ist    s _       -> locs' [s]
  Iand   _ s0 s1   -> locs' [s0, s1]
  Iandi  _ s0 _    -> locs' [s0]
  Ior    _ s0 s1   -> locs' [s0, s1]
  Iori   _ s0 _    -> locs' [s0]
  Ixor   _ s0 s1   -> locs' [s0, s1]
  Ixori  _ s0 _    -> locs' [s0]
  Pnot   _ s       -> locs' [s]
  Islt   _ s0 s1   -> locs' [s0, s1]
  Islti  _ s0 _    -> locs' [s0]
  Isltu  _ s0 s1   -> locs' [s0, s1]
  Isltiu _ s0 _    -> locs' [s0]
  Psne   _ s0 s1   -> locs' [s0, s1]
  Pseq   _ s0 s1   -> locs' [s0, s1]
  Psle   _ s0 s1   -> locs' [s0, s1]
  Ibranch _        -> Set.empty
  IcondBranch _ _  -> Set.empty
  Icall _          -> Set.empty  -- TODO: consider argument registers (A0–A7)

writeLocs :: Instr -> Set Arg
writeLocs (Ilabel _ instr) = writeLocs instr
writeLocs instr = case instr of
  Iadd   d _ _     -> locs' [d]
  Iaddi  d _ _     -> locs' [d]
  Ineg   d _       -> locs' [d]
  Isub   d _ _     -> locs' [d]
  Pmv    d _       -> locs' [d]
  Ild    d _       -> locs' [d]
  Ili    d _       -> locs' [d]
  Ist    _ d       -> locs' [d]
  Iand   d _ _     -> locs' [d]
  Iandi  d _ _     -> locs' [d]
  Ior    d _ _     -> locs' [d]
  Iori   d _ _     -> locs' [d]
  Ixor   d _ _     -> locs' [d]
  Ixori  d _ _     -> locs' [d]
  Pnot   d _       -> locs' [d]
  Islt   d _ _     -> locs' [d]
  Islti  d _ _     -> locs' [d]
  Isltu  d _ _     -> locs' [d]
  Isltiu d _ _     -> locs' [d]
  Psne   d _ _     -> locs' [d]
  Pseq   d _ _     -> locs' [d]
  Psle   d _ _     -> locs' [d]
  Ibranch _        -> Set.empty
  IcondBranch _ _  -> Set.empty
  Icall _          -> locs' [ArgReg A0] -- return register

liveBefore :: Instr -> Set Arg -> Set Arg
liveBefore instr s = s & flip Set.difference (writeLocs instr) & Set.union (readLocs instr)

splitBlocks :: [Instr] -> [(String, [Instr])]
splitBlocks (i@(Ilabel lbl _):is) =
  let (l', is', bs) = foldl go (lbl, [i], []) is in
    reverse $ (l', reverse is') : bs
  where
    go (lbl, iAcc, bAcc) i@(Ilabel lbl' _) =
      let bAcc' = (lbl, reverse iAcc) : bAcc
      in (lbl', [i], bAcc')
    go (lbl, iAcc, bAcc) i = (lbl, i:iAcc, bAcc)
splitBlocks _ = throw $ MyException "instruction sequence must start with a label"

replaceVars :: Map Arg Arg -> Instr -> Instr
replaceVars m (Ilabel lbl i) = Ilabel lbl (replaceVars m i)
replaceVars m instr =
  let re v = Map.findWithDefault v v m
  in case instr of
    Iadd   d s0 s1   -> Iadd   (re d) (re s0) (re s1)
    Iaddi  d s0 s1   -> Iaddi  (re d) (re s0) (re s1)
    Ineg   d s       -> Ineg   (re d) (re s)
    Isub   d s0 s1   -> Isub   (re d) (re s0) (re s1)
  
    Pmv    d s       -> Pmv    (re d) (re s)
    Ild    d s       -> Ild    (re d) (re s)
    Ili    d s       -> Ili    (re d) (re s)
    Ist    s d       -> Ist    (re s) (re d)
  
    Iand   d s0 s1   -> Iand   (re d) (re s0) (re s1)
    Iandi  d s0 s1   -> Iandi  (re d) (re s0) (re s1)
    Ior    d s0 s1   -> Ior    (re d) (re s0) (re s1)
    Iori   d s0 s1   -> Iori   (re d) (re s0) (re s1)
    Ixor   d s0 s1   -> Ixor   (re d) (re s0) (re s1)
    Ixori  d s0 s1   -> Ixori  (re d) (re s0) (re s1)
    Pnot   d s       -> Pnot   (re d) (re s)
  
    Islt   d s0 s1   -> Islt   (re d) (re s0) (re s1)
    Islti  d s0 s1   -> Islti  (re d) (re s0) (re s1)
    Isltu  d s0 s1   -> Isltu  (re d) (re s0) (re s1)
    Isltiu d s0 s1   -> Isltiu (re d) (re s0) (re s1)
  
    Psne   d s0 s1   -> Psne   (re d) (re s0) (re s1)
    Pseq   d s0 s1   -> Pseq   (re d) (re s0) (re s1)
    Psle   d s0 s1   -> Psle   (re d) (re s0) (re s1)
    IcondBranch cond lbl ->
      let cond' = case cond of
            Beq a b -> Beq (re a) (re b)
            Beqz a -> Beqz (re a)
            Bne a b -> Bne (re a) (re b)
            Bnez a -> Bnez (re a)
            Blt a b -> Blt (re a) (re b)
            Bge a b -> Bge (re a) (re b)
      in IcondBranch cond' lbl
    Ibranch _; Icall _ -> instr
