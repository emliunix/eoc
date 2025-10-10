module Lang.Eoc.Asm.Interference where

import Lang.Eoc.Types (MyException(..), PassM)
import Lang.Eoc.Asm.Types

import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe, maybeToList)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

interferes :: Instr -> Set Arg -> [(Arg, Arg)]
interferes (IMv d s) lives =
  [ (d, r)
  | r <- Set.toList lives
  , r /= d && r /= s
  ]
interferes (ICall _) lives =
  [ (r1, r2)
  | r1 <- map ArgReg callerSavedRegs
  , r2 <- Set.toList lives
  , r1 /= r2
  ]
interferes i lives =
  [ (r1, r2)
  | r1 <- Set.toList $ writeLocs i
  , r2 <- Set.toList lives
  , r1 /= r2
  ]

buildFromBlock :: [Instr] -> Set Arg -> [(Arg, Arg)]
buildFromBlock instrs liveAfter =
  let lives = foldr (\i acc@(lives:_) -> (liveBefore i lives) : acc) [liveAfter] instrs
  in concat $ zipWith interferes instrs (tail lives)

-- | precomputed values:
-- 
-- state:
-- 1. assigned colors: Map Var Color
--    since all registers are pre-colored, when miss, it must be a variable
-- 2. inference graph: Map Var (Set Var)
-- 3. unassigned variables: [Var]
-- computed:
-- 1. unassigned -> saturation
dSatur :: Map Arg (Set Arg) -> Map Arg Int -> [Arg] -> Map Arg Int
dSatur graph colorMap uncolored =
  go
    (Map.fromList [(a, saturation colorMap a) | a <- uncolored])
    colorMap
  where
    saturation colorMap x =
      let adjs = Map.findWithDefault Set.empty x graph
      in mapMaybe (`Map.lookup` colorMap) (Set.toList adjs)
    pickColor adjColors =
      go 0 (IS.fromList adjColors)
      where
        go c used
          | Just c == IS.lookupGE c used = go (c+1) used
          | otherwise = c
    go sats colors | Map.null sats = colors
    go sats colors =
      -- find the most saturated variable
      let (x, adjColors) = maximumBy
            (comparing (length . snd))
            $ Map.toList sats
          -- assign the lowest possible color
          colors' = let xCol = pickColor adjColors
                    in  Map.insert x xCol colors
          -- delete x from sats
          -- update all neighbors of x new saturation
          sats' = foldl go (Map.delete x sats) (concat $ maybeToList $ Set.toList <$> Map.lookup x graph)
            where go sats v = Map.insert v (saturation colors' v) sats
      in go sats' colors'

buildInterferenceGraph :: Asm -> PassM Asm
buildInterferenceGraph _ = error "not implemented"
