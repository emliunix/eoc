module Lang.Eoc.Parser where

import Control.Exception (throw)
import Text.Trifecta (Result (..))

import Lang.Eoc.Sexp
import Lang.Eoc.Types (MyException(..), parsePrimOp)
import Lang.Eoc.R.Types

parseR :: Sexp -> R
parseR s = Program Info (parseExp s)

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

parseRfromString :: String -> Result R
parseRfromString input = case parseSexpFromString input of
  Success exp -> Success $ parseR exp
  Failure failure -> Failure failure

parseRfromString' :: String -> R
parseRfromString' input = case parseRfromString input of
  Success r -> r
  Failure err -> throw $ MyException $ "parse error: " ++ show err
