module Lang.Eoc.Asm.Moves where

import Control.Monad (forM_, forM)
import qualified Data.Equivalence.Monad as E

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.Types (PassM, MyException(..))
import Lang.Eoc.Asm.Types

moves :: [Instr] -> [(Arg, Arg)]
moves [] = []
moves (Ilabel _ i : is) = moves (i:is)
moves (Pmv d s : is) = (d, s) : moves is
moves (_ : is) = moves is

uncoverMoves' :: [Instr] -> Map Arg (Set Arg)
uncoverMoves' instrs = foldl goMv Map.empty (moves instrs)
  where goMv acc (v1, v2) =
          Map.insertWith Set.union v1 (Set.singleton v2) $
            Map.insertWith Set.union v2 (Set.singleton v1) acc

uncoverMoves :: Asm -> PassM Asm
uncoverMoves (AsmProgram info instrs) = pure $ AsmProgram info { aiMoves = Just (uncoverMoves' instrs) } instrs
