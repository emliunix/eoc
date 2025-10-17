module Lang.Eoc.C.Types where

import Control.Exception (throw)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (StateT(..), gets, modify)
import Control.Monad.State.Class (MonadState)

import Data.List (intercalate)

import Data.Map (Map)
import qualified Data.Map as Map

import Lang.Eoc.Types (Var, PrimOp(..), MyException(..), PassM, execPass, freshBlock)
import Lang.Eoc.R.Types

data CType
  = CTyInt
  | CTyBool
  | CTyUnit
  deriving (Show)

data CAtm = CInt Int | CBool Bool | CUnit | CVar Var
  deriving (Eq, Ord)

instance Show CAtm where
  show (CInt i) = show i
  show (CBool b) = show b
  show CUnit = "'()"
  show (CVar v) = v

data CExp = CAtm CAtm | CPrim PrimOp [CAtm] | CFunRef Var | CCall CAtm [CAtm]

instance Show CExp where
  show (CAtm atm) = show atm
  show (CPrim op args) = "(" ++ show op ++ " " ++ (unwords . map show $ args) ++ ")"
  show (CFunRef var) = "<funref " ++ var ++ ">"
  show (CCall fn args) = "(" ++ show fn ++ " " ++ (unwords . map show $ args) ++ ")"

data Stmt
  = Assign Var CExp
  | StmtPrim PrimOp [CAtm]
  | StmtCall CAtm [CAtm]

instance Show Stmt where
  show (Assign var exp) = "  " ++ var ++ " = " ++ show exp
  show (StmtPrim op args) = "  " ++ unwords (show op : map show args)
  show (StmtCall fn args) = "   (" ++ show fn ++ " " ++ unwords (map show args) ++ ")"

data Tail
  = Return CExp
  -- | TailCall CAtm [CAtm]
  | Seq Stmt Tail
  | Goto Label
  | IfStmt PrimOp CAtm CAtm Tail Tail

instance Show Tail where
  show (Return exp) = "  return " ++ show exp
  -- show (TailCall fn args) = "  (" ++ show fn ++ " " ++ unwords (map show args) ++ ")"
  show (Seq stmt tail) = show stmt ++ "\n" ++ show tail
  show (Goto lbl) = "  goto "++ lbl
  show (IfStmt cmp a b thn els) =
    let cmpab = unwords [show cmp, show a, show b]
    in case (thn, els) of
      (Goto thn', Goto els') -> "  if (" ++ cmpab  ++ ") then goto " ++ thn' ++ " else goto " ++ els'
      _ -> "  __invalid if (" ++ cmpab ++ ") then {\n" ++ show thn ++ "\n  } else {\n" ++ show els ++ "\n  }"

data CInfo = CInfo { }
  deriving (Show)

type Label = String

data C = CProgram CInfo [(Label, Tail)]

data CDefInfo = CDefInfo { }
  deriving (Show)

data CDef = CDef CDefInfo Var [(Var, CType)] CType [(Label, Tail)]

data CDefs = CDefsProgram CInfo [CDef]

instance Show C where
  show (CProgram _ tails) =
    unlines $ map (\(lbl, tail) -> lbl ++ ":\n" ++ show tail) tails

instance Show CDef where
  show (CDef _ name args retTy tails) =
    let body = unlines $ map (\(lbl, tail) -> lbl ++ ":\n" ++ show tail) tails
        args' = (intercalate "," $ map (\(v, ty) -> v ++ ": " ++ show ty) args)
        header = name ++ "(" ++ args' ++ ") -> " ++ show retTy
    in header ++ "\n" ++ body

instance Show CDefs where
  show (CDefsProgram _ defs) =
    unlines $ map show defs

data CPassState = CPassState
  { blocks :: Map Label Tail
  }

newtype CPassT m a = CPassT { runCPassT :: StateT CPassState m a }
  deriving (Functor, Applicative, Monad, MonadState CPassState)

instance MonadTrans CPassT where
  lift = CPassT . lift

runCPass :: Monad m => CPassT m a -> m (a, CPassState)
runCPass (CPassT m) = runStateT m (CPassState Map.empty)

createBlock :: Monad m => Label -> CPassT m Tail -> CPassT m Tail
createBlock lbl tail = do
  blk <- gets (Map.lookup lbl . blocks)
  case blk of
    Just blk -> return blk
    Nothing -> do
      tail <- tail
      modify $ \s -> s { blocks = Map.insert lbl tail (blocks s) }
      return tail

gotoBlock :: Monad m => Label -> CPassT m Tail -> CPassT m Tail
gotoBlock lbl tail = do
  tail <- createBlock lbl tail
  case tail of
    g@(Goto _) -> return g
    _ -> return $ Goto lbl

type CPass a = CPassT PassM a

-- helper functions

gotos :: Tail -> [Label]
gotos (Seq _ t) = gotos t
gotos (Goto lbl) = [lbl]
gotos (IfStmt _ _ _ t1 t2) = gotos t1 ++ gotos t2
gotos (Return _) = []
