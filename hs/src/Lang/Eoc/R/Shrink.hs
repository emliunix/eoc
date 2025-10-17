module Lang.Eoc.R.Shrink where

import Control.Exception (throw)
import Data.Function (fix)

import Lang.Eoc.Types
import Lang.Eoc.R.Types

shrink :: RDefsExp -> PassM RDefs
shrink (RDefsExpProgram info defs exp) =
  let defs' = createMain exp : defs
      defs'' = map shrinkDef defs'
  in return $ RDefsProgram info defs''
  where
    shrinkDef (Def defInfo name args ty exp) =
      Def defInfo name args ty (shrinkExp exp)

shrinkExp :: Exp -> Exp
shrinkExp =
 fix handle
 where
   handle recur (Prim PrimAnd [a, b]) =
     If (recur a) (recur b) (Bool_ False)
   handle recur (Prim PrimOr [a, b]) =
     If (recur a) (Bool_ True) (recur b)
   handle recur e = recurExp recur e

createMain :: Exp -> Def
createMain = Def (DefInfo { }) "main" [] TyInt
