module Lang.Eoc.R.Rco where

import Control.Monad.Fix (mfix)
import Control.Exception (throw)

import Lang.Eoc.Types 
import Lang.Eoc.R.Types

removeComplexOperands :: R -> PassM R
removeComplexOperands (Program info exp) = do
  Program info <$> rcoExpr exp

rcoExpr :: Exp -> PassM Exp
-- we only need to handle Prim specially
rcoExpr (Prim op args) = do
  (args', depss) <- unzip <$> traverse rcoAtm args
  let deps = concat depss
  return $ foldl (\body (v, e) -> Let v e body) (Prim op args') deps
-- the followings are simply to recurse
rcoExpr (Let var exp body) = do
  exp' <- rcoExpr exp
  body' <- rcoExpr body
  return $ Let var exp' body'
rcoExpr (If cond thn els) = do
  cond' <- rcoExpr cond
  thn' <- rcoExpr thn
  els' <- rcoExpr els
  return $ If cond' thn' els'
rcoExpr (SetBang var exp) = do
  SetBang var <$> rcoExpr exp
rcoExpr (Begin exps body) = do
  exps' <- traverse rcoExpr exps
  body' <- rcoExpr body
  return $ Begin exps' body'
rcoExpr (While cond body) = do
  cond' <- rcoExpr cond
  body' <- rcoExpr body
  return $ While cond' body'
-- atoms and GetBang are leaves
rcoExpr e = return e

-- | make sure it's an atom
-- | otherwise create a tmp var binding to make it an atom
-- | I doubt if we really need rcoAtm, it's really simple
rcoAtm :: Exp -> PassM (Exp, [(Var, Exp)])
rcoAtm t@(Int_ _) = return (t, [])
rcoAtm t@(Bool_ _) = return (t, [])
rcoAtm t@Unit_ = return (t, [])
rcoAtm t@(Var _) = return (t, [])
rcoAtm e = do
  e' <- rcoExpr e
  tmpv <- freshTmpVar
  return (Var tmpv, [(tmpv, e')])
