module Lang.Eoc.Typecheck where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Exception (throw)

import Lang.Eoc.Types
import Lang.Eoc.R.Types

typeCheck :: RDefs -> RDefs
typeCheck (RDefsProgram info defs) =
  let
    defTy (Def _ name args retTy _) = (name, TyFun (map snd args) retTy)
    defsTy = map defTy defs
    tyckDef (Def info name args retTy body) =
      let (body', bodyTy) = typeCheckExp (args ++ defsTy) body
      in throwExcept $ do
        check (bodyTy == retTy) $
          "Type mismatch in function " ++ name ++ ": expected " ++ show retTy ++
           ", but got " ++ show bodyTy
        return $ Def info name args retTy body'
    defs' = map tyckDef defs
  in RDefsProgram info defs'

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
  let (cond', condTy) = typeCheckExp env cond
      (thn', thnTy) = typeCheckExp env thn
      (els', elsTy) = typeCheckExp env els
  in throwExcept $ do
    check (condTy /= TyBool) $ "Condition of if must be Bool, but got " ++ show condTy
    check (thnTy /= elsTy) $ "Branches of if must have the same type," ++
      " but got " ++ show thnTy ++ " and " ++ show elsTy
    return (If cond' thn' els', thnTy)
typeCheckExp env (SetBang var exp) =
  let expTy = lookupEnv var env
      (exp', expTy') = typeCheckExp env exp
  in throwExcept $ do
    check (expTy /= expTy') $ "Type mismatch in set!, variable " ++ var ++
      " has type " ++ show expTy ++ ", but got " ++ show expTy'
    return (SetBang var exp', TyUnit)
typeCheckExp env (GetBang var) =
  let ty = lookupEnv var env
  in (GetBang var, ty)
typeCheckExp env (Begin exps body) =
  let (exps', _) = unzip $ map (typeCheckExp env) exps
      (body', bodyTy) = typeCheckExp env body
  in (Begin exps' body', bodyTy)
typeCheckExp env (While cond body) =
  let (cond', condTy) = typeCheckExp env cond
      (body', bodyTy) = typeCheckExp env body
  in throwExcept $ do
    check (condTy /= TyBool) $ "Condition of while must be Bool, but got " ++ show condTy
    check (bodyTy /= TyUnit) $ "Body of while must be Unit, but got " ++ show bodyTy
    return (While cond' body', TyUnit)
typeCheckExp env (Apply fun args) =
  let (args', argsTy) = unzip $ map (typeCheckExp env) args
      (fun', funTy) = typeCheckExp env fun
  in throwExcept $ do
    case funTy of
      TyFun argsTy' retTy -> do
        check (argsTy == argsTy') $ "Function argument type mismatch: expected " ++
          show argsTy' ++ ", but got " ++ show argsTy
        return (Apply fun' args', retTy)
      _ -> throwError $ "Expected function type, but got " ++ show funTy
typeCheckExp _ t@(FunRef _) = throw $ MyException $ "Unexpected internal FunRef: " ++ show t

typeCheckPrimOp :: PrimOp -> [Ty] -> Ty
typeCheckPrimOp PrimRead   [] = TyInt
typeCheckPrimOp PrimNeg    [TyInt] = TyInt
typeCheckPrimOp PrimPlus   [TyInt, TyInt] = TyInt
typeCheckPrimOp PrimSub    [TyInt, TyInt] = TyInt
typeCheckPrimOp PrimEq     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimEq     [TyBool, TyBool] = TyBool
typeCheckPrimOp PrimEq     [TyUnit, TyUnit] = TyBool
typeCheckPrimOp PrimNe     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimNe     [TyBool, TyBool] = TyBool
typeCheckPrimOp PrimLt     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimLe     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimGt     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimGe     [TyInt, TyInt] = TyBool
typeCheckPrimOp PrimAnd    [TyBool, TyBool] = TyBool
typeCheckPrimOp PrimOr     [TyBool, TyBool] = TyBool
typeCheckPrimOp PrimNot    [TyBool] = TyBool
-- TODO: type check vector
typeCheckPrimOp op         argTys = throw $ MyException $ "Type error in primitive operation: " ++ show op ++ " with args " ++ show argTys

throwExcept :: Except String a -> a
throwExcept ex = case runExcept ex of
  Left msg -> throw $ MyException msg
  Right val -> val

check :: Bool -> String -> Except String ()
check True _ = return ()
check False ~msg = throwError msg
