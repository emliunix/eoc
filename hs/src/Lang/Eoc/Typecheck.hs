module Lang.Eoc.Typecheck where

import Control.Exception (throw)
import Data.Function ((&))

import Lang.Eoc.Types
import Lang.Eoc.R

typeCheck :: R -> (R, Ty)
typeCheck (Program info exp) =
  let (exp', ty) = typeCheckExp [] exp
  in (Program info exp', ty)

type Env = [(Var, Ty)]

lookupEnv :: Var -> Env -> Ty
lookupEnv var env =
  case lookup var env of
    Just ty -> ty
    Nothing -> throw $ MyException $ var ++ " not found"

typeCheckExp :: Env -> Exp -> (Exp, Ty)
typeCheckExp _   t@(Int_ _)  = (t, TyInt)
typeCheckExp _   t@(Bool_ _) = (t, TyBool)
typeCheckExp _   Unit_ = (Unit_, TyUnit)
typeCheckExp env t@(Var var) =
  case lookup var env of
    Just ty -> (t, ty)
    Nothing -> throw $ MyException $ var ++ " not found"
typeCheckExp env (Let var exp body) =
  let (exp', ty) = typeCheckExp env exp
      env' = (var, ty) : env
      (body', bodyTy) = typeCheckExp env' body
  in (Let var exp' body', bodyTy)
typeCheckExp env (Prim op args) =
  let
    (args', tys) = unzip $ map (typeCheckExp env) args
    ty = typeCheckPrimOp op tys
  in (Prim op args', ty)
typeCheckExp env (If cond thn els) =
  let (cond', _) = typeCheckExp env cond &
        expectTy (\ty -> ty == TyBool || throw (
                     MyException $ "Condition of if must be Bool, but got " ++ show ty))
      (thn', thnTy) = typeCheckExp env thn
      (els', _) = typeCheckExp env els &
        expectTy (\elsTy -> elsTy == thnTy || throw (
                     MyException $ "Branches of if must have the same type," ++
                     " but got " ++ show thnTy ++ " and " ++ show elsTy))
  in (If cond' thn' els', thnTy)
typeCheckExp env (SetBang var exp) =
  let expTy = lookupEnv var env
      (exp', _) = typeCheckExp env exp &
        expectTy (\ty -> ty == expTy || throw (
                     MyException $ "Type mismatch in set!, variable " ++ var ++
                     " has type " ++ show expTy ++
                     ", but got " ++ show ty))
  in (SetBang var exp', TyUnit)
typeCheckExp env (GetBang var) =
  let ty = lookupEnv var env
  in (GetBang var, ty)
typeCheckExp env (Begin exps body) =
  let (exps', _) = unzip $ map (typeCheckExp env) exps
      (body', bodyTy) = typeCheckExp env body
  in (Begin exps' body', bodyTy)
typeCheckExp env (While cond body) =
  let (cond', _) = typeCheckExp env cond &
        expectTy (\ty -> ty == TyBool || throw (
                     MyException $ "Condition of while must be Bool, but got " ++ show ty))
      (body', _) = typeCheckExp env body
  in (While cond' body', TyUnit)

expectTy :: (Ty -> Bool) -> (Exp, Ty) -> (Exp, Ty)
expectTy p t@(_, ty) = p ty `seq` t

typeCheckPrimOp :: PrimOp -> [Ty] -> Ty
typeCheckPrimOp PrimRead   [] = TyInt
typeCheckPrimOp PrimNeg    [TyInt] = TyInt
typeCheckPrimOp PrimPlus   [TyInt, TyInt] = TyInt
typeCheckPrimOp PrimSub    [TyInt, TyInt] = TyInt
typeCheckPrimOp PrimEq     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimLt     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimLe     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimGt     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimGe     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimAnd    [TyBool, TyBool] = TyBool
typeCheckPrimOp PrimOr     [TyBool, TyBool] = TyBool
typeCheckPrimOp PrimNot    [TyBool] = TyBool
typeCheckPrimOp op         argTys = throw $ MyException $ "Type error in primitive operation: " ++ show op ++ " with args " ++ show argTys
