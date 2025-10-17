module Lang.Eoc.R.RevealFunctions where

import Data.Function (fix)

import Lang.Eoc.Types
import Lang.Eoc.R.Types

revealFunctions' :: [Var] -> Exp -> Exp
revealFunctions' funcDefs = fix go
  where
    go _ (Var v) | v `elem` funcDefs = FunRef v
                 | otherwise = Var v
    go recur e = recurExp recur e

revealFunctions :: RDefs -> PassM RDefs
revealFunctions (RDefsProgram info defs) = pure $ RDefsProgram info defs'
  where
    funcNames = [ name | Def _ name _ _ _ <- defs ]
    defs' = [ Def defInfo name args retTy (revealFunctions' funcNames body)
            | Def defInfo name args retTy body <- defs ]
