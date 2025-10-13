module Lang.Eoc.Types where

import Control.Monad.State (State, state, evalState)
import Control.Exception (Exception)

import Data.Set (Set)
import qualified Data.Set as Set

type Var = String

data PrimOp
  = PrimRead
  | PrimNeg
  | PrimPlus
  | PrimSub
  | PrimEq
  | PrimNe
  | PrimLt
  | PrimLe
  | PrimGt
  | PrimGe
  | PrimAnd -- pseudo
  | PrimOr -- pseudo
  | PrimNot -- pseudo, translated to xori
  | PrimXori
  | PrimVector
  | PrimVectorLen
  deriving (Eq, Ord)

instance Show PrimOp where
  show PrimRead = "read"
  show PrimNeg = "-"
  show PrimPlus = "+"
  show PrimSub = "-"
  show PrimEq = "=="
  show PrimNe = "/="
  show PrimLt = "<"
  show PrimLe = "<="
  show PrimGt = ">"
  show PrimGe = ">="
  show PrimAnd = "and" -- pseudo
  show PrimOr = "or" -- pseudo
  show PrimNot = "not"
  show PrimXori = "xori"
  show PrimVector = "vector"
  show PrimVectorLen = "vector-length"

parsePrimOp :: String -> Maybe PrimOp
parsePrimOp "read" = Just PrimRead
parsePrimOp "-" = Just PrimNeg
parsePrimOp "+" = Just PrimPlus
parsePrimOp "==" = Just PrimEq
parsePrimOp "/=" = Just PrimNe
parsePrimOp "<" = Just PrimLt
parsePrimOp "<=" = Just PrimLe
parsePrimOp ">" = Just PrimGt
parsePrimOp ">=" = Just PrimGe
parsePrimOp "and" = Just PrimAnd
parsePrimOp "or" = Just PrimOr
parsePrimOp "not" = Just PrimNot
parsePrimOp "vector" = Just PrimVector
parsePrimOp "vector-length" = Just PrimVectorLen
parsePrimOp _ = Nothing

cmpOpSet' :: Set PrimOp
cmpOpSet' = Set.fromList [PrimEq, PrimNe, PrimLt, PrimLe, PrimGt, PrimGe]

isCmpOp :: PrimOp -> Bool
isCmpOp op
  | op `Set.member` cmpOpSet' = True
  | otherwise = False

newtype MyException = MyException String
  deriving (Show)
instance Exception MyException

-- TODO: I'm considering make it a PassMonad for general transformation pass neeeds
newtype Gensym a = Gensym { runGensym :: State Int a }
  deriving (Functor, Applicative, Monad)

evalGensym :: Gensym a -> a
evalGensym = flip evalState 0 . runGensym

gensym :: Gensym Int
gensym = Gensym $ state (\i -> (i, i + 1))

data PassState = PassState
  { nextVarId :: Int
  , nextTmpVarId :: Int
  , nextBlockId :: Int
  }

newtype PassM a = PassM { runPassM :: State PassState a }
  deriving (Functor, Applicative, Monad)

execPass :: PassM a -> a
execPass m = evalState (runPassM m) (PassState 0 0 0)

freshVar :: String -> PassM Var
freshVar prefix = PassM $ state $ \s ->
  let varId = nextVarId s
      var = prefix ++ show varId
      s' = s { nextVarId = varId + 1 }
  in (var, s')

freshTmpVar :: PassM Var
freshTmpVar = PassM $ state $ \s ->
  let varId = nextTmpVarId s
      var = "tmp" ++ show varId
      s' = s { nextTmpVarId = varId + 1 }
  in (var, s')

freshBlock :: PassM String
freshBlock = PassM $ state $ \s ->
  let blockId = nextBlockId s
      block = "block" ++ show blockId
      s' = s { nextBlockId = blockId + 1 }
  in (block, s')
