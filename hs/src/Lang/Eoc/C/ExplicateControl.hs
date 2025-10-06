{-|

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
module Lang.Eoc.C.ExplicateControl where

import Control.Exception (throw)
import Control.Monad (join)
import Control.Monad.Trans (lift)

import Data.Map (Map)
import qualified Data.Map as Map

import Lang.Eoc.Types
import Lang.Eoc.R.Types
import Lang.Eoc.C.Types

catm :: Exp -> CAtm
catm (Int_ i) = CInt i
catm (Bool_ b) = CBool b
catm Unit_ = CVar "unit"  -- represent unit as a variable named "unit"
catm (Var var) = CVar var
catm a = throw $ MyException $ "expected an atom, got " ++ show a

cexpr :: Exp -> CExp
cexpr (Prim op atms) = CPrim op $ map catm atms
cexpr e = CAtm $ catm e

freshBlock' :: CPass Label
freshBlock' = lift freshBlock

block :: CPass Tail -> CPass (CPass Tail)
block tail = do
  lbl <- lift freshBlock
  return $ gotoBlock lbl tail

explicateTail :: Exp -> CPass Tail
explicateTail (Let var exp body) = explicateAssign var exp $ explicateTail body
explicateTail (If cond thn els) =
  join $ explicatePred cond
    <$> block (explicateTail thn)
    <*> block (explicateTail els)
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
    _ -> Seq (Assign var (cexpr e)) <$> cont

explicatePred :: Exp -> CPass Tail -> CPass Tail -> CPass Tail
explicatePred cond thn els =
  case cond of
    (Bool_ b) -> if b then thn else els
    (Var v) -> explicatePred (Prim PrimEq [Var v, Bool_ True]) thn els
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
    _ -> throw $ MyException $ "invalid if condition: " ++ show cond

explicateControl :: R -> PassM C
explicateControl (Program _ exp) = do
  (tail, state) <- runCPass $ explicateTail exp
  return $ CProgram (CInfo {}) (("start", tail) : Map.toList (blocks state))
