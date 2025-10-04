module Lang.Eoc.R.Shrink where

import Control.Exception (throw)
import Data.Function (fix)

import Lang.Eoc.Types
import Lang.Eoc.R.Types

shrink :: R -> PassM R
shrink (Program info exp) = do
  let exp' = shrinkExp exp
  return $ Program info exp'

shrinkExp :: Exp -> Exp
shrinkExp =
 fix handle
 where
   handle recur (Prim PrimAnd [a, b]) =
     If (recur a) (recur b) (Bool_ False)
   handle recur (Prim PrimOr [a, b]) =
     If (recur a) (Bool_ True) (recur b)
   handle recur (Prim op args) =
     let args' = fmap recur args
     in Prim op args'
   handle recur e = recurExp recur e

-- | Currently no primop shrink needs
shrinkPrimOp :: PrimOp -> [Exp] -> Exp
-- shrinkPrimOp PrimNe [a, b] = Prim PrimNot [Prim PrimEq [a, b]]
-- shrinkPrimOp PrimLte [a, b] = Prim PrimNot [Prim PrimLt [b, a]]
-- shrinkPrimOp PrimGte [a, b] = Prim PrimNot [Prim PrimLt [a, b]]
-- shrinkPrimOp PrimGt [a, b] = Prim PrimLt [b, a]
-- shrinkPrimOp PrimSub [a, b] = Prim PrimPlus [a, Prim PrimNeg [b]]
-- shrinkPrimOp op args = Prim op args
shrinkPrimOp = Prim
