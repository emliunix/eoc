module Lang.Eoc.C.ExplicateControl where
{-^

The explicate-control pass.

= mental model

I found this part hard to understand. So I created some mental models to help understanding.

__Base Case__: it has to be clear we have already compiled expr with rco so basically a expr is either:

* a simple expr: an atm or a prim op call taking atms
* a complex: either a Let binding or a If statement

To handle nested complexes, we can split the cases: let-let, let-if, if-let, if-if.
We only need to focus on the right hand side of let, and the condition position of if.

It's not to match 2 levels deep. The cases are recursively matched, so it handles arbitrary dpeth of nesting.

The book suggested we create auxiliary functions:

* explicate-tail
* explicate-assign
* explicate-pred
* explicate-effect

explicate-tail dispatches let expr to explicate-assign, and if expr to explicate-pred.

When at explicate-assign, we case split on expr so we can tell it's let-let or let-if.

When at explicate-pred, we case split on cond so we can tell it's if-let or if-if.

Assignments and conditional jumping are special in CPU instructions, or say, have special meaning in CFG.

This pass visually reorganizes these structures but the ordering is strictly preserved.

= freshBlock

We pass blocks as @CPass Tail@ for the purpose of lazy evaluation.

The label is the key to identify created blocks. With Haskell Monad, we can defer block creation, and label manipulation is a compuation, so you have ensted Monads.

First, `block` creates the label immediately, but the computation of tail is not executed, instead, it's wrapped with `gotoBlock` and `createBlock`. When the block is actually needed, the previously label is used to check if the block is already created, and create if it is not.

Because evaluate the block may in term call `block` internally. So care should be taken to avoid duplicate evaluation. Otherwise, you have multiple labels and duplicated generated blocks.

This is guaranteed by `createBlock` which won't call the computation if the block is already created.

-}
import Control.Exception (throw)
import Control.Monad (join)
import Control.Monad.Trans (lift)

import Data.Bifunctor (second)

import Data.Map (Map)
import qualified Data.Map as Map

import Lang.Eoc.Types
import Lang.Eoc.R.Types
import Lang.Eoc.C.Types

ctype :: Ty -> CType
ctype TyInt = CTyInt
ctype TyBool = CTyBool
ctype TyUnit = CTyUnit
ctype ty = throw $ MyException $ "cannot convert to CType: " ++ show ty

catm :: Exp -> CAtm
catm (Int_ i) = CInt i
catm (Bool_ b) = CBool b
catm Unit_ = CUnit
catm (Var var) = CVar var
catm (GetBang var) = CVar var
catm a = throw $ MyException $ "expected an atom, got " ++ show a

cexpr :: Exp -> CExp
cexpr (Prim op atms) = CPrim op $ map catm atms
cexpr (FunRef f) = CFunRef f
cexpr (Apply fun args) = CCall (catm fun) (map catm args)
cexpr e = CAtm $ catm e

freshBlock' :: CPass Label
freshBlock' = lift freshBlock

block :: CPass Tail -> CPass (CPass Tail)
block tail = do
  lbl <- lift freshBlock
  return $ gotoBlock lbl tail

explicateTail :: Exp -> CPass Tail
explicateTail (Let var exp body) = explicateAssign var exp $ explicateTail body
explicateTail (SetBang var e) = explicateAssign var e $ explicateTail Unit_
explicateTail (If cond thn els) =
  join $ explicatePred cond
    <$> block (explicateTail thn)
    <*> block (explicateTail els)
explicateTail (Begin exps body) = explicateEffects exps $ explicateTail body
explicateTail w@(While _ _) = explicateEffect w (explicateTail Unit_)
-- explicateTail (Apply fun args) =
--   return $ TailCall (catm fun) (map catm args)
explicateTail expr = return $ Return $ cexpr expr

explicateAssign :: Var -> Exp -> CPass Tail -> CPass Tail
explicateAssign var e cont =
  case e of
    Let var' e' body ->
      explicateAssign var' e' $ explicateAssign var body cont
    If cond thn els -> do
      cont' <- block cont
      join $ explicatePred cond
        <$> block (explicateAssign var thn cont')
        <*> block (explicateAssign var els cont')
    Begin exps body -> explicateEffects exps (explicateAssign var body cont)
    (SetBang var' e') -> explicateAssign var' e' cont
    w@(While _ _) -> explicateEffect w cont
    _ -> Seq (Assign var (cexpr e)) <$> cont

explicatePred :: Exp -> CPass Tail -> CPass Tail -> CPass Tail
explicatePred cond thn els =
  case cond of
    (Bool_ b) -> if b then thn else els
    (Var v) -> explicatePred (Prim PrimEq [Var v, Bool_ True]) thn els
    (GetBang v) -> explicatePred (Prim PrimEq [Var v, Bool_ True]) thn els
    (Prim op [a, b]) ->
      IfStmt op (catm a) (catm b)
        <$> thn
        <*> els
    (If cond' thn' els') ->
      join $ explicatePred cond'
        <$> block (explicatePred thn' thn els)
        <*> block (explicatePred els' thn els)
    (Let var exp body) ->
      explicateAssign var exp $ explicatePred body thn els
    (Begin exps body) -> explicateEffects exps (explicatePred body thn els)
    _ -> throw $ MyException $ "invalid if condition: " ++ show cond

explicateEffect :: Exp -> CPass Tail -> CPass Tail
explicateEffect expr cont =
  case expr of
    -- discard non-effectful expressions
    (Int_ i) -> cont
    (Bool_ b) -> cont
    Unit_ -> cont
    (Var v) -> cont
    (FunRef f) -> cont
    (GetBang v) -> cont
    -- except for read
    (Prim PrimRead []) -> Seq (StmtPrim PrimRead []) <$> cont
    (Prim op args) -> cont
    (Apply fun args) -> Seq (StmtCall (catm fun) (map catm args)) <$> cont
    -- complex expressions
    (SetBang var e) -> explicateAssign var e cont
    (Let var e body) -> explicateAssign var e $ explicateEffect body cont
    (If cond thn els) -> do
      cont' <- block cont
      join $ explicatePred cond
        <$> block (explicateEffect thn cont')
        <*> block (explicateEffect els cont')
    (Begin exps body) -> explicateEffects exps $ explicateEffect body cont
    (While cond body) -> do
      loopLbl <- freshBlock'
      cont' <- block cont
      body' <- block $ explicateEffect body (pure $ Goto loopLbl)
      let body'' = explicatePred cond
                  body'
                  cont'
      _ <- createBlock loopLbl body''
      return $ Goto loopLbl

explicateEffects :: [Exp] -> CPass Tail -> CPass Tail
explicateEffects = flip $ foldr explicateEffect

explicateControl :: RDefs -> PassM CDefs
explicateControl (RDefsProgram _ defs) = do
  CDefsProgram (CInfo {}) <$> traverse goDef defs
  where
    goDef (Def _ name args retTy body) = do
      (tail, state) <- runCPass $ explicateTail body
      startLbl <- ("start" ++) . show <$> freshBlockId
      let blocks' = (startLbl, tail) : Map.toList (blocks state)
      let defInfo = CDefInfo
            { startBlockLabel = startLbl
            }
      return $ CDef defInfo name args' retTy' blocks'
      where
        args' = map (second ctype) args
        retTy' = ctype retTy
