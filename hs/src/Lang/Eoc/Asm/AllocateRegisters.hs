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
-- -2 -> RA
-- -3 -> SP
reservedRegs :: [Reg]
reservedRegs = [S0, RA, SP]

-- | registers free for allocation,
-- 0 indexed
freeRegs :: [Reg]
freeRegs =
  [ A0, A1, A2, A3, A4, A5, A6, A7
  , T3, T4, T5, T6
  , S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11
  ]

-- | the index of S1 in freeRegs
index1stSaved :: Int
index1stSaved = 12

sizeSaved :: Int
sizeSaved = 11

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

allocateRegisters :: AsmDefs -> PassM AsmDefs
allocateRegisters (AsmDefsProgram info defs) = AsmDefsProgram info <$> traverse goDef defs
  where
    goDef (AsmDef info name instrs) =
      let
        interferenceGraph = case aiInterferences info of
          Just g -> g
          Nothing -> throw $ MyException "no interference graph found in AsmInfo"
        movesGraph = case aiMoves info of
          Just mvs -> mvs
          Nothing -> throw $ MyException "no moves found in AsmInfo"
        initialColors = Map.fromList $ zip (map ArgReg reservedRegs) (map negate [1..])
        colors = dSatur interferenceGraph initialColors movesGraph
        maxCol = foldl max 0 $ Map.elems colors
        stackSlots =
          let nSlots = maxCol - length freeRegs
          in if nSlots > 0
             then map (\i -> ArgMemRef (i*8) SP) [0..nSlots]
             else []
        coloredSlots = map ArgReg freeRegs ++ stackSlots
        stackSpace = ((16 + (length stackSlots * 8) + 15) `div` 16) * 16
        usedSavedRegs =
          let nUsed = min sizeSaved (max 0 (maxCol - index1stSaved))
          in take nUsed $ drop index1stSaved freeRegs
        instrs' = map (replaceVars $ Map.map (coloredSlots !!) colors) instrs
        info' = info
          { aiStackSpace = Just stackSpace
          , aiUsedSavedRegs = Just usedSavedRegs
          }
      in return $ AsmDef info' name instrs'
