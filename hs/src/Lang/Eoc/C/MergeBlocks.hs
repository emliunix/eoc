{-|
Merge blocks: run this pass after PlaceBlocks, so we can leverage its ordering.
-}
module Lang.Eoc.C.MergeBlocks
  ( mergeBlocks
  ) where

import Control.Exception (throw)

import Data.Maybe (maybeToList)
import Data.Graph (Graph)
import qualified Data.Graph as Graph

import Lang.Eoc.Types
import Lang.Eoc.C.Types

-- | check only unconditional gotos
-- If-Stmt requires it's branches to be Goto so it's not mergeable
goto :: Tail -> Maybe Label
goto (Seq _ t) = goto t
goto (Goto lbl) = Just lbl
goto _ = Nothing

replaceGoto :: Tail -> Tail -> Tail
replaceGoto (Seq stmt t) t' = Seq stmt (replaceGoto t t')
replaceGoto (Goto _) t' = t'
replaceGoto t _ = throw $ MyException $ "expected a goto, got " ++ show t

merge :: [(Label, Tail)] -> [(Label, Tail)]
merge blocks =
  let (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges
        [ (blk, lbl, filter (/= lbl) $ maybeToList $ goto blk)
        | (lbl, blk) <- blocks
        ]
      graphT = Graph.transposeG graph
      go [] = []
      go ((lbl, blk):xs) =
        let
          trees = Graph.dfs graphT $ maybeToList $ vertexFromKey lbl
        in case trees of
          [Graph.Node _ [Graph.Node v _]]
            | (blk', lbl', _) <- nodeFromVertex v ->
                -- only one dominator
                (lbl, replaceGoto blk blk') : go (filter ((/= lbl') . fst) xs)
          _ -> (lbl, blk) : go xs
  in go blocks

mergeBlocks :: CDefs -> PassM CDefs
mergeBlocks (CDefsProgram info defs) =
  CDefsProgram info <$> defs'
  where
    defs' = traverse mergeDef defs
    mergeDef (CDef info name args rty blocks) =
      return $ CDef info name args rty (merge blocks)
