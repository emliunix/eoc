module Lang.Eoc.Parser where

import Control.Exception (throw)
import Text.Trifecta (Result (..))

import Lang.Eoc.Sexp
import Lang.Eoc.Types (MyException(..), parsePrimOp)
import Lang.Eoc.R.Types

parseR :: [Sexp] -> RDefsExp
parseR sexps = RDefsExpProgram Info defs' exp
  where
    go [] _ = throw $ MyException "no expression found"
    go [sexp] defs = (reverse defs, parseExp sexp)
    go (x:xs) defs = go xs (parseDef x : defs)
    (defs', exp) = go sexps []

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
parseExp (List [Symbol "let", List [List [Symbol v, e]], b]) =
      --  List [Symbol "let", List [List [Symbol "x",Integer 10]], e]
  Let v (parseExp e) (parseExp b)
parseExp (List [Symbol "if", cond, thn, els]) =
  If (parseExp cond) (parseExp thn) (parseExp els)
parseExp (List ((Symbol "begin"):exps)) =
  let (exps', body) = splitAtLast exps
  in Begin (map parseExp exps') (parseExp body)
parseExp (List [Symbol "while", cond, body]) =
  While (parseExp cond) (parseExp body)
parseExp (List [Symbol "set!", Symbol v, e]) =
  SetBang v (parseExp e)
parseExp (List [Symbol "get!", Symbol v]) =
  GetBang v
parseExp (List (Symbol op:args))
  | Just op' <- parsePrimOp op = Prim op' (map parseExp args)
  | otherwise = throw $ MyException $ "unknown operator: " ++ op
parseExp sexp = throw $ MyException $ "cannot parse expression: " ++ show sexp

splitAtLast :: [a] -> ([a], a)
splitAtLast [] = throw $ MyException "empty list"
splitAtLast [x] = ([], x)
splitAtLast (x:xs) =
  let (xs', z) = splitAtLast xs
  in (x:xs', z)

parseRfromString :: String -> Result RDefsExp
parseRfromString input = case parseSexpsFromString input of
  Success exps -> Success $ parseR exps
  Failure failure -> Failure failure

parseRfromString' :: String -> RDefsExp
parseRfromString' input = case parseRfromString input of
  Success r -> r
  Failure err -> throw $ MyException $ "parse error: " ++ show err
