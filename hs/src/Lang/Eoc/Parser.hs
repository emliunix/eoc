module Lang.Eoc.Parser where

import Control.Exception (throw)
import Text.Trifecta (Result (..))

import Lang.Eoc.Sexp
import Lang.Eoc.Types (MyException(..), parsePrimOp, PrimOp(..))
import Lang.Eoc.R.Types
import Data.List (unsnoc)

parseR :: [Sexp] -> RDefsExp
parseR sexps = RDefsExpProgram Info defs' exp'
  where
    (defs, exp) = case unsnoc sexps of
      Just x -> x
      Nothing -> throw $ MyException "no expression found"
    defs' = map parseDef defs
    exp' = parseExp exp

parseDef :: Sexp -> Def
parseDef (List [Symbol "define", List (Symbol name:args), Colon, retTy, body]) =
  Def DefInfo name args' retTy' body'
  where
    retTy' = parseTy retTy
    body' = parseExp body
    args' = map parseArg args
    parseArg (List [Symbol v, Colon, ty])
      | ty' <- parseTy ty = (v, ty')
    parseArg sexp = throw $ MyException $ "cannot parse argument: " ++ show sexp
parseDef sexp = throw $ MyException $ "cannot parse definition: " ++ show sexp

parseTy :: Sexp -> Ty
parseTy (Symbol "Int") = TyInt
parseTy (Symbol "Bool") = TyBool
parseTy (Symbol "Unit") = TyUnit
parseTy (List (ty:Symbol "->":xs)) =
  case parseTy (List xs) of
    TyFun args ret -> TyFun (parseTy ty : args) ret
    ret -> TyFun [parseTy ty] ret
parseTy sexp = throw $ MyException $ "cannot parse type: " ++ show sexp

parseExp :: Sexp -> Exp
parseExp (Integer i) = Int_ $ fromIntegral i
parseExp (Bool b) = Bool_ b
parseExp Nil = Unit_
parseExp (Symbol v) = Var v
parseExp (String s) = throw $ MyException $ "string not supported: " ++ s
parseExp (List (Symbol "let":xs)) =
  case xs of
    [List defs, b] -> foldr go (parseExp b) defs
      where
        go (List [Symbol v, e]) body = Let v (parseExp e) body
        go sexp _ = throw $ MyException $ "cannot parse let binding: " ++ show sexp
    _ -> throw $ MyException $ "invalid let expression: " ++ show xs
parseExp (List (Symbol "if":xs)) =
  case xs of
    [cond, thn, els] -> If (parseExp cond) (parseExp thn) (parseExp els)
    _ -> throw $ MyException $ "invalid if expression: " ++ show xs
parseExp (List ((Symbol "begin"):exps)) =
  case unsnoc $ map parseExp exps of
    Just (exps', body) -> Begin exps' body
    Nothing -> throw $ MyException "begin must have at least one expression"
parseExp (List t@(Symbol "while":xs)) =
  case xs of
    [cond, body] -> While (parseExp cond) (parseExp body)
    _ -> throw $ MyException $ "invalid while expression: " ++ show t
parseExp (List (Symbol "set!":xs)) =
  case xs of
    [Symbol v, e] -> SetBang v (parseExp e)
    _ -> throw $ MyException $ "invalid set! expression: " ++ show xs
parseExp (List (Symbol "get!":xs)) =
  case xs of
    [Symbol v] -> GetBang v
    _ -> throw $ MyException $ "invalid get! expression: " ++ show xs
parseExp (List (Symbol op:args))
  | op == "-" = case args of
      [e] -> Prim PrimNeg [parseExp e]
      [e1, e2] -> Prim PrimSub [parseExp e1, parseExp e2]
      _ -> throw $ MyException $ "invalid number of arguments to -: " ++ show (length args)
  | Just op' <- parsePrimOp op = Prim op' (map parseExp args)
  | otherwise = Apply (Var op) (map parseExp args)
parseExp sexp = throw $ MyException $ "cannot parse expression: " ++ show sexp

parseRfromString :: String -> Result RDefsExp
parseRfromString input = case parseSexpsFromString input of
  Success exps -> Success $ parseR exps
  Failure failure -> Failure failure

parseRfromString' :: String -> RDefsExp
parseRfromString' input = case parseRfromString input of
  Success r -> r
  Failure err -> throw $ MyException $ "parse error: " ++ show err
