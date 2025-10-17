module Lang.Eoc.R.UncoverGet
  ( uncoverGet
  ) where

import Data.Function (fix)
import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.Types 
import Lang.Eoc.R.Types

scanSetExp :: Exp -> Set Var
scanSetExp (SetBang var exp) =
  Set.insert var (scanSetExp exp)
scanSetExp (Let _ exp body) =
  Set.union (scanSetExp exp) (scanSetExp body)
scanSetExp (Prim _ args) =
  Set.unions (map scanSetExp args)
scanSetExp (If cond thn els) =
  Set.unions [scanSetExp cond, scanSetExp thn, scanSetExp els]
scanSetExp (Begin exps body) =
  Set.unions (map scanSetExp (body:exps))
scanSetExp (While cond body) =
  Set.union (scanSetExp cond) (scanSetExp body)
scanSetExp _ = Set.empty

uncoverGet :: RDefs -> PassM RDefs
uncoverGet (RDefsProgram info defs) =
  return $ RDefsProgram info defs'
  where
    goDef (Def info name args ty exp) =
      Def info name args ty (uncoverGetExp setVars exp)
      where
        setVars = scanSetExp exp
        exp' = uncoverGetExp setVars exp
    defs' = map goDef defs

uncoverGetExp :: Set Var -> Exp -> Exp
uncoverGetExp setVars =
  fix handle
  where
    handle _ (Var var) =
      if Set.member var setVars
        then GetBang var
        else Var var
    handle recur e =
      recurExp recur e
