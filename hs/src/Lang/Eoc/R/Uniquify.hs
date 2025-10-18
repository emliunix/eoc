{-# LANGUAGE OrPatterns #-}
module Lang.Eoc.R.Uniquify where

import Debug.Trace (trace)

import Control.Exception (throw)

import Lang.Eoc.Types
import Lang.Eoc.R.Types

uniquify :: RDefs -> PassM RDefs
uniquify (RDefsProgram info defs) = do
  defs' <- traverse uniquifyDef defs
  return $ RDefsProgram info defs'
  where
    defsEnv = [(name, name) | Def _ name _ _ _ <- defs]
    uniquifyDef (Def info name args retTy body) =
      let argsEnv = [(v, v) | (v, _) <- args]
      in Def info name args retTy <$> uniquifyExp (argsEnv ++ defsEnv) body

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
uniquifyExp env (Apply func args) = do
  func' <- uniquifyExp env func
  args' <- traverse (uniquifyExp env) args
  return $ Apply func' args'
-- FunRef is internal only, so we can't even encounter it now
uniquifyExp _ t@(FunRef _) = return t
uniquifyExp _ t@(Int_ _; Bool_ _; Unit_) = return t
