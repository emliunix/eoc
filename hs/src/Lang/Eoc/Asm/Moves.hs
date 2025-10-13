module Lang.Eoc.Asm.Moves where

import Control.Monad (forM_, forM)
import qualified Data.Equivalence.Monad as E

import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.Types (PassM, MyException(..))
import Lang.Eoc.Asm.Types

moves :: [Instr] -> [(Arg, Arg)]
moves [] = []
moves (Ilabel _ i : is) = moves (i:is)
moves (Pmv d s : is) = (d, s) : moves is
moves (_ : is) = moves is

uncoverMoves :: Asm -> PassM Asm
uncoverMoves (AsmProgram info blocks) = pure $ AsmProgram info { aiMoves = Just movesSet } blocks
  where
    movesSet = E.runEquivM Set.singleton Set.union $ do
      forM_ (moves blocks) $ \(a, b) -> do
        E.equate a b
      classes <- E.classes
      forM classes E.desc
