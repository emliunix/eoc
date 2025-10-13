module Lang.Eoc.Asm.AllocateRegisters where

import Control.Arrow ((&&&))
import Control.Exception (throw)

import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe, maybeToList, fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Lang.Eoc.Types (MyException(..), PassM)
import Lang.Eoc.Asm.Types

-- | negatively numbered reserved registers
-- -1 -> S0(FP)
-- -2 -> A1
-- and so on
reservedRegs :: [Reg]
reservedRegs = [S0, T6]

-- | registers free for allocation,
-- 0 indexed
freeRegs :: [Reg]
freeRegs =
  [ A0, A1, A2, A3, A4, A5, A6, A7
  , T0, T1, T2, T3, T4, T5
  , S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11
  ]

-- | dsatur graph coloring with move biasing
dSatur :: (Ord t, Show t) =>
  Map t (Set t) -> -- ^ interference graph
  Map t Int -> -- ^ pre-colored variables
  Map t (Set t) -> -- ^ moves graph for move biasing
  Map t Int -- ^ all colored variables
dSatur graph colorMap moves =
  go
    (Map.fromList [(a, saturation colorMap a) | a <- uncolored])
    colorMap
  where
    uncolored = [ k | k <- Map.keys graph, Map.notMember k colorMap ]
    saturation colorMap x =
      let adjs = Map.findWithDefault Set.empty x graph
      in mapMaybe (`Map.lookup` colorMap) (Set.toList adjs)
    pickColor v adjColors colorMap =
      let moveAffineColors = mapMaybe (`Map.lookup` colorMap) $ Set.toList $ fromMaybe Set.empty $ Map.lookup v moves
      in case filter (`IS.notMember` adjColors) moveAffineColors of
           (c:_) -> c
           [] -> head $ filter (`IS.notMember` adjColors) [0..]
    go sats colors | Map.null sats = colors
    go sats colors =
      let
        -- find the most saturated variable
        (x, adjColors) = maximumBy
                         (comparing (length . snd &&& affineMove . fst))
                         $ Map.toList sats
          where
            affineMove v = maybe 0 Set.size (Map.lookup v moves)
        -- assign color
        colors' = let xCol = pickColor x (IS.fromList adjColors) colors
                  in  Map.insert x xCol colors
        -- delete x from sats and
        -- update all neighbors of x with new saturation
        sats' = foldl go (Map.delete x sats) neighbours
            where
              go sats v = Map.insert v (saturation colors' v) sats
              neighs = Map.findWithDefault Set.empty x graph
              neighbours = filter (`Set.member` neighs) $ Map.keys sats
      in go sats' colors'

allocateRegisters :: Asm -> PassM Asm
allocateRegisters (AsmProgram info instrs) = pure $ AsmProgram info instrs'
  where
    interferenceGraph = case aiInferences info of
      Just g -> g
      Nothing -> throw $ MyException "no interference graph found in AsmInfo"
    movesGraph = case aiMoves info of
      Just mvs -> Map.fromList [ (v, s) | s <- mvs, v <- Set.toList s ]
      Nothing -> throw $ MyException "no moves found in AsmInfo"
    initialColors = Map.fromList $ zip (map ArgReg reservedRegs) (map negate [1..])
    colors = dSatur interferenceGraph initialColors movesGraph
    -- TODO: stack slots allocation
    -- 1. size/align of each slot
    -- 2. computes stack space and store to info
    -- TODO: handle stack slots
    instrs' = map (replaceVars $ Map.map (map ArgReg freeRegs !!) colors) instrs
