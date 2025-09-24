module Lang.Eoc.Types where

import Control.Monad.State (State, state, evalState)
import Control.Exception (Exception)

type Var = String

data PrimOp
  = PrimRead
  | PrimNeg
  | PrimPlus

instance Show PrimOp where
  show PrimRead = "read"
  show PrimNeg = "-"
  show PrimPlus = "+"

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
