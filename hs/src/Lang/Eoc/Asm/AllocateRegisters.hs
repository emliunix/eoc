module Lang.Eoc.Asm.AllocateRegisters where

import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe, maybeToList)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Lang.Eoc.Types (MyException(..), PassM)
import Lang.Eoc.Asm.Types

dSatur :: (Ord t, Show t) => Map t (Set t) -> Map t Int -> Map t Int
dSatur graph colorMap =
  go
    (Map.fromList [(a, saturation colorMap a) | a <- uncolored])
    colorMap
  where
    uncolored = [ k | k <- Map.keys graph, Map.notMember k colorMap ]
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
      let (x, adjColors) = maximumBy -- find the most saturated variable
                           (comparing (length . snd))
                           $ Map.toList sats
          -- assign the lowest possible color
          colors' = let xCol = pickColor adjColors
                    in  Map.insert x xCol colors
          -- delete x from sats
          -- update all neighbors of x new saturation
          sats' = foldl go (Map.delete x sats)
                  (concat $ maybeToList
                   $ Set.toList . (Set.\\ (Set.fromList $ Map.keys colors'))
                   <$> Map.lookup x graph)
            where go sats v = Map.insert v (saturation colors' v) sats
      in go sats' colors'

allocateRegisters :: Asm -> PassM Asm
allocateRegisters _ = error "not implemented"
