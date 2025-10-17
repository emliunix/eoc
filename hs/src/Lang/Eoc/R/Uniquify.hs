module Lang.Eoc.R.Uniquify where

import Control.Exception (throw)

import Lang.Eoc.Types
import Lang.Eoc.R.Types

uniquify :: RDefs -> PassM RDefs
uniquify (RDefsProgram info defs) = do
  defs' <- traverse uniquifyDef defs 
  return $ RDefsProgram info defs'
  where
    uniquifyDef (Def info name args retTy body) =
      Def info name args retTy <$> uniquifyExp [] body

type NamesEnv = [(Var, Var)]

lookupNamesEnv :: Var -> NamesEnv -> Var
lookupNamesEnv var env = case lookup var env of
  Just x -> x
  Nothing -> throw $ MyException $ var ++ " not found"

uniquifyExp :: NamesEnv -> Exp -> PassM Exp
uniquifyExp env (Var var) = return $ Var (lookupNamesEnv var env)
uniquifyExp env (Let var exp body) = do
  exp' <- uniquifyExp env exp
  var' <- freshVar var
  body' <- uniquifyExp ((var, var') : env) body
  return $ Let var' exp' body'
uniquifyExp env (Prim op args) = do
  args' <- traverse (uniquifyExp env) args
  return $ Prim op args'
uniquifyExp env (If cond thn els) = do
  cond' <- uniquifyExp env cond
  thn' <- uniquifyExp env thn
  els' <- uniquifyExp env els
  return $ If cond' thn' els'
uniquifyExp env (SetBang var exp) = do
  exp' <- uniquifyExp env exp
  return $ SetBang (lookupNamesEnv var env) exp'
uniquifyExp env (GetBang var) =
  return $ GetBang (lookupNamesEnv var env)
uniquifyExp env (Begin exps body) = do
  exps' <- traverse (uniquifyExp env) exps
  body' <- uniquifyExp env body
  return $ Begin exps' body'
uniquifyExp env (While cond body) = do
  cond' <- uniquifyExp env cond
  body' <- uniquifyExp env body
  return $ While cond' body'
uniquifyExp _ t = return t
