module Lang.Eoc.C.Types where

import Control.Exception (throw)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (StateT(..), gets, modify)
import Control.Monad.State.Class (MonadState)

import Data.Map (Map)
import qualified Data.Map as Map

import Lang.Eoc.Types (Var, PrimOp(..), MyException(..), PassM, execPass, freshBlock)
import Lang.Eoc.R.Types

data CType
  = CTyInt
  | CTyBool
  | CTyUnit
  deriving (Show)

data CAtm = CInt Int | CBool Bool | CVar Var

instance Show CAtm where
  show (CInt i) = show i
  show (CBool b) = show b
  show (CVar v) = v

data CExp = CAtm CAtm | CPrim PrimOp [CAtm]

instance Show CExp where
  show (CAtm atm) = show atm
  show (CPrim op args) = "(" ++ show op ++ " " ++ (unwords . map show $ args) ++ ")"

data Stmt
  = Assign Var CExp

instance Show Stmt where
  show (Assign var exp) = "  " ++ var ++ " = " ++ show exp

data Tail
  = Return CExp
  | Seq Stmt Tail
  | Goto Label
  | IfStmt PrimOp CAtm CAtm Tail Tail

instance Show Tail where
  show (Return exp) = "  return " ++ show exp
  show (Seq stmt tail) = show stmt ++ "\n" ++ show tail
  show (Goto lbl) = "  goto "++ lbl
  show (IfStmt cmp a b thn els) = "  if (" ++ unwords [show cmp, show a, show b]  ++ ") " ++ show thn ++ " " ++ show els

data CInfo = CInfo { }
  deriving (Show)

type Label = String

data C = CProgram CInfo [(Label, Tail)]

instance Show C where
  show (CProgram _ tails) =
    unlines $ map (\(lbl, tail) -> lbl ++ ":\n" ++ show tail) tails

data CPassState = CPassState
  { blocks :: Map Label Tail
  }

newtype CPassT m a = CPassT { runCPassT :: StateT CPassState m a }
  deriving (Functor, Applicative, Monad, MonadState CPassState)

instance MonadTrans CPassT where
  lift = CPassT . lift

runCPass :: Monad m => CPassT m a -> m (a, CPassState)
runCPass (CPassT m) = runStateT m (CPassState Map.empty)

createBlock :: Monad m => Label -> Tail -> CPassT m ()
createBlock lbl tail = do
  blk <- gets (Map.lookup lbl . blocks)
  case blk of
    Just _ -> return ()
    Nothing -> do
      modify $ \s -> s { blocks = Map.insert lbl tail (blocks s) }
      return ()

lazyBlock :: Monad m => Label -> Tail -> CPassT m Tail
lazyBlock _ g@(Goto _) = return g
lazyBlock lbl tail = do
  createBlock lbl tail
  return $ Goto lbl

type CPass a = CPassT PassM a
