module Lang.Eoc.CVar where

import Control.Exception (throw)
import Control.Monad.State (modify, execState)

import Data.Map (Map)
import qualified Data.Map as Map

import Lang.Eoc.Types (Var, PrimOp(..), MyException(..))
import Lang.Eoc.RVar

-- CVar

data CAtm = CInt Int | CVar Var

instance Show CAtm where
  show (CInt i) = show i
  show (CVar v) = v

data CExp = CAtm CAtm | CPrim PrimOp [CAtm]

instance Show CExp where
  show (CAtm atm) = show atm
  show (CPrim op args) = "(" ++ show op ++ " " ++ (unwords . map show $ args) ++ ")"

data Stmt
  = Assign Var CExp

instance Show Stmt where
  show (Assign var exp) = var ++ " = " ++ show exp

data Tail
  = Return CExp
  | Seq Stmt Tail

instance Show Tail where
  show (Return exp) = "return " ++ show exp
  show (Seq stmt tail) = show stmt ++ "\n" ++ show tail

data CVarInfo = CVarInfo { }
  deriving (Show)

type Label = String

data CVar
  = CProgram CVarInfo [(Label, Tail)]
  deriving (Show)

data CType
  = CTyInt
  deriving (Show)

-- pass: explicate-control

catm :: Exp -> CAtm
catm (Int_ i) = CInt i
catm (Var var) = CVar var
catm a = throw $ MyException $ "expected an atom, got " ++ show a

cexpr :: Exp -> CExp
cexpr (Prim op atms) = CPrim op $ map catm atms
cexpr e = CAtm $ catm e

explicateTail :: Exp -> Tail
explicateTail (Let var exp body) = explicateAssign var exp $ explicateTail body
explicateTail atm = Return $ cexpr atm

explicateAssign :: Var -> Exp -> Tail -> Tail
explicateAssign var e cont =
  case e of
    Let var' e' body ->
      explicateAssign var' e' $ explicateAssign var body cont
    _ -> Seq (Assign var (cexpr e)) cont

explicateControl :: RVar -> CVar
explicateControl (Program info exp) =
  CProgram (CVarInfo {}) [("start", explicateTail exp)]

example6 = Let "a" (Let "b" (Int_ 42) (Var "b")) (Prim PrimPlus [Int_ 3, Var "a"])
example7 = Let "y" (Let "x" (Int_ 20)
                    (Prim PrimPlus [Var "x", Let "x" (Int_ 22) (Var "x")]))
           (Var "y")

typeCheckCVar :: CVar -> Map Var CType
typeCheckCVar (CProgram _ [(_, tail)]) = execState (tyck tail) Map.empty
  where
    tyck (Seq (Assign v e) tail) = do
      put v CTyInt
      tyck tail
    tyck (Return e) = return ()
    put v ty = modify $ \m -> Map.insert v ty m
