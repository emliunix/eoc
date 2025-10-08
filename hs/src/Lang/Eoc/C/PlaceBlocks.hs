{-|
PlaceBlocks in Reverse PostOrder.

And the real effect is to simply put the then branch right after the if condition.

And since if (while compiles to if too) is the majority source of blocks.
The algorithm now works just for if.

-}
module Lang.Eoc.C.PlaceBlocks
  ( placeBlocks
  ) where

import Control.Exception (throw)

import Data.Maybe (catMaybes)
import Data.Graph (Graph)
import qualified Data.Graph as Graph

import Lang.Eoc.Types
import Lang.Eoc.C.Types

postOrder :: Graph.Tree a -> [a]
postOrder (Graph.Node a xs) = concatMap postOrder (reverse xs) ++ [a]

rpoBlocks :: String -> [(Label, Tail)] -> [(Label, Tail)]
rpoBlocks entry blocks =
  let
    (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges
      [ (blk, lbl, gotos blk)
      | (lbl, blk) <- blocks
      ]
    [tree] = Graph.dfs graph $ catMaybes [vertexFromKey entry]
  in
    [ (l, t)
    | (t, l, _) <- map nodeFromVertex $ reverse $ postOrder tree
    ]

placeBlocks :: C -> PassM C
placeBlocks (CProgram info blocks) = return $ CProgram info $ rpoBlocks "start" blocks
