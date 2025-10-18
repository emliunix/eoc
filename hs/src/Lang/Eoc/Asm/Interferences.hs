module Lang.Eoc.Asm.Interferences where

import Lang.Eoc.Types (MyException(..), PassM)
import Lang.Eoc.Asm.Types

import Control.Exception (throw)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

interferes :: Instr -> Set Arg -> [(Arg, Arg)]
interferes (Pmv d s) lives =
  [ (d, r)
  | r <- Set.toList lives
  , r /= d && r /= s
  ]
interferes i lives =
  [ (r1, r2)
  | r1 <- Set.toList $ writeLocs i
  , r2 <- Set.toList lives
  , r1 /= r2
  ]

buildFromBlock :: [Instr] -> Set Arg -> [(Arg, Arg)]
buildFromBlock instrs liveAfter =
  let lives = foldr (\i acc@(lives:_) -> liveBefore i lives : acc) [liveAfter] instrs
  in concat $ zipWith interferes instrs (tail lives)

buildInterferences' :: [(String, [Instr])] -> Map String (Set Arg) -> Map Arg (Set Arg)
buildInterferences' blocks livesMap =
  foldl goEdge Map.empty edges
  where
    edges = foldl goBlk [] blocks
    goBlk acc (lbl, blk) =
      let livesAfter = case Map.lookup lbl livesMap of
            Just m -> m
            Nothing -> throw $ MyException $ "Liveness information missing for block " ++ lbl
      in buildFromBlock blk livesAfter ++ acc
    goEdge m (v1, v2) =
      Map.insertWith Set.union v1 (Set.singleton v2) $
        Map.insertWith Set.union v2 (Set.singleton v1) m


buildInterferences :: AsmDefs -> PassM AsmDefs
buildInterferences (AsmDefsProgram info defs) =
  AsmDefsProgram info <$> traverse goDef defs
  where
    goDef (AsmDef info name instrs) =
      let
        livesMap = case aiLivesMap info of
          Just lm -> lm
          Nothing -> throw $ MyException
            "Liveness information missing when building interference graph"
        interferenceGraph = buildInterferences' (splitBlocks instrs) livesMap
      in return $ AsmDef (info { aiInterferences = Just interferenceGraph }) name instrs
