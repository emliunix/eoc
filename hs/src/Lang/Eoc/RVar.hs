module Lang.Eoc.RVar where

import Control.Exception (throw)

import Lang.Eoc.Types (Var, PrimOp(..), MyException(..), Gensym, gensym, evalGensym)

-- RVar

data RVarInfo = RVarInfo { }
  deriving Show

data RVar
  = Program RVarInfo Exp
  deriving Show

data Exp
  = Int_ Int
  | Prim PrimOp [Exp]
  | Var Var
  | Let Var Exp Exp

instance Show Exp where
  show (Int_ i) = show i
  show (Prim op []) = "(" ++ show op ++ ")"
  show (Prim op args) = "(" ++ show op ++ " " ++ (unwords . map show $ args) ++ ")"
  show (Var var) = var
  show (Let var exp body) = "(let ([" ++ var ++ " " ++ show exp ++ "]) " ++ show body ++ ")"

-- pass: uniquify

uniquifyExp :: [(Var, Var)] -> Exp -> Gensym Exp
uniquifyExp _ (Int_ i) = return $ Int_ i
uniquifyExp env (Prim op args) = do
  args' <- sequenceA [uniquifyExp env arg | arg <- args]
  return $ Prim op args'
uniquifyExp env (Let var exp body) = do
  exp' <- uniquifyExp env exp
  sym <- gensym
  let var' = var ++ "." ++ show sym
  body' <- uniquifyExp ((var, var') : env) body
  return $ Let var' exp' body'
uniquifyExp env (Var var) = do
  let var' = case lookup var env of
        Just x -> x
        Nothing -> throw $ MyException $ var ++ " not found"
  return $ Var var'

uniquify :: RVar -> Gensym RVar
uniquify (Program info exp) = Program info <$> uniquifyExp [] exp

example1 = Let "x" (Prim PrimPlus [Let "x" (Int_ 1) (Var "x"), Int_ 2]) (Prim PrimNeg [Var "x"])

example2 = Let "x" (Int_ 32)
  (Prim PrimPlus [Let "x" (Int_ 10) (Var "x"), Var "x"])

example3 = Let "x" (Let "x" (Int_ 4)
                    (Prim PrimPlus [Var "x", Int_ 1]))
           (Prim PrimPlus [Var "x", Int_ 2])

-- pass: remove complex opera*
-- to ANF

-- make expr it atomic
rcoAtom :: Exp -> Gensym (Exp, [(Var, Exp)])
rcoAtom (Prim op args) = do
    (args, defss) <- unzip <$> traverse rcoAtom args
    var <- ("tmp." ++) . show <$> gensym
    return (Var var, (var, Prim op args) : concat defss)
rcoAtom (Let var exp body) = do
  (c, defs) <- rcoAtom body
  exp' <- rcoExpr exp
  return (c, (var, exp') : defs)
-- for already atomic exp
rcoAtom exp = return (exp, [])

rcoExpr :: Exp -> Gensym Exp
rcoExpr (Prim op args) = do
  (args, defss) <- unzip <$> traverse rcoAtom args
  return $ foldl (\c (v, e) -> Let v e c) (Prim op args) $ concat defss
rcoExpr (Let var exp body) = do
  exp' <- rcoExpr exp
  body' <- rcoExpr body
  return $ Let var exp' body'
rcoExpr expr = return expr

removeComplexOperators :: RVar -> Gensym RVar
removeComplexOperators (Program info exp) = Program info <$> rcoExpr exp

example4 = Let "a" (Int_ 42) (Let "b" (Var "a") (Var "b"))

example5 = Prim PrimNeg [Let "a" (Int_ 42) (Var "a")]
