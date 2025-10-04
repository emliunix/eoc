module Lang.Eoc.R.Types where

import Control.Exception (throw)

import Lang.Eoc.Types (Var, PrimOp(..), MyException(..), Gensym, gensym, evalGensym)

-- RVar

data Info = Info { }
  deriving (Show, Eq)

data Ty = TyInt | TyBool | TyUnit | TyFun [Ty] Ty
  deriving (Eq)

instance Show Ty where
  show TyInt = "Int"
  show TyBool = "Bool"
  show TyUnit = "Unit"
  show (TyFun args ret) = "(" ++ (unwords . map show $ args ) ++ " -> " ++ show ret ++ ")"

data R
  = Program Info Exp
  deriving (Show, Eq)

data Exp
  = Int_ Int
  | Bool_ Bool
  | Unit_
  | Var Var
  | Prim PrimOp [Exp]
  | Let Var Exp Exp
  | If Exp Exp Exp
  | SetBang Var Exp
  | GetBang Var
  | Begin [Exp] Exp
  | While Exp Exp
  deriving (Eq)

instance Show Exp where
  show (Int_ i) = show i
  show (Bool_ b) = if b then "#t" else "#f"
  show Unit_ = "'()"
  show (Prim op []) = "(" ++ show op ++ ")"
  show (Prim op args) = "(" ++ show op ++ " " ++ (unwords . map show $ args) ++ ")"
  show (Var var) = var
  show (Let var exp body) = "(let ([" ++ var ++ " " ++ show exp ++ "]) " ++ show body ++ ")"
  show (If cond thn els) = "(if " ++ show cond ++ " "
    ++ show thn ++ " "
    ++ show els ++ ")"
  show (SetBang var exp) = "(set! " ++ var ++ " " ++ show exp ++ ")"
  show (GetBang var) = "(get! " ++ var ++ ")"
  show (Begin exps body) = "(begin " ++ (unwords . map show $ exps) ++ " " ++ show body ++ ")"
  show (While cond body) = "(while " ++ show cond ++ " " ++ show body ++ ")"

recurExp :: (Exp -> Exp) -> Exp -> Exp
recurExp recur (Prim op args) = Prim op (map recur args)
recurExp recur (Let var exp body) = Let var (recur exp) (recur body)
recurExp recur (If cond thn els) = If (recur cond) (recur thn) (recur els)
recurExp recur (SetBang var exp) = SetBang var (recur exp)
recurExp recur (Begin exps body) = Begin (map recur exps) (recur body)
recurExp recur (While cond body) = While (recur cond) (recur body)
recurExp _ t = t

recurExpM :: Monad m => (Exp -> m Exp) -> Exp -> m Exp
recurExpM recur (Prim op args) = Prim op <$> mapM recur args
recurExpM recur (Let var exp body) = Let var <$> recur exp <*> recur body
recurExpM recur (If cond thn els) = If <$> recur cond <*> recur thn <*> recur els
recurExpM recur (SetBang var exp) = SetBang var <$> recur exp
recurExpM recur (Begin exps body) = Begin <$> mapM recur exps <*> recur body
recurExpM recur (While cond body) = While <$> recur cond <*> recur body
recurExpM _ t = return t
