module Lang.Eoc.Asm.Liveness where

import Control.Exception (throw)

import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Stack (HasCallStack, prettyCallStack, callStack)

import Lang.Eoc.Types (PassM, MyException(..))
import Lang.Eoc.Asm.Types

gotos :: [Instr] -> [String]
gotos [] = []
gotos (Ilabel _ i : is) = gotos (i:is)
gotos (Ibranch lbl : is) = lbl : gotos is
gotos (IcondBranch _ lbl : is) = lbl : gotos is
gotos (_ : is) = gotos is

multiMap :: (Ord k, Ord v) => [(k, v)] -> Map k (Set v)
multiMap xs =
  go xs Map.empty
  where
    go [] m = m
    go ((k, v):kvs) m =
      go kvs $ Map.insertWith Set.union k (Set.singleton v) m

analyzeLiveness' :: [(String, [Instr])] -> Map String (Set Arg) -> [(String, Set Arg)]
analyzeLiveness' blocks inits =
  let
    edges = [ (lbl, dst) | (lbl, instrs) <- blocks, dst <- gotos instrs ]
    codes = Map.fromList blocks
    preds = multiMap [ (dst, src) | (src, dst) <- edges ]
    succs = multiMap edges
    getLivesAfter m lbl =
      foldl
        (\s sucLbl -> let
                s' = lookupMap' sucLbl m
                in Set.union s s')
        Set.empty
        (lookupMap' lbl succs)
    go m [] =
      -- recalculates livesAfter for each label
      map ((\l -> (l, getLivesAfter m l)) . fst) blocks
    go m (l:ls) = let
      -- livesAfter is the union of all successors' livesBefore
      livesAfter = getLivesAfter m l
      -- the new livesBefore by applying liveBefore to the new livesAfter
      lives' = foldr liveBefore livesAfter (lookupMap' l codes)
      -- the old livesBefore
      lives = lookupMap' l m
      in if lives /= lives'
         then
           let m' = Map.insert l lives' m
               ls' = Set.toList (Map.findWithDefault Set.empty l preds) ++ ls
           in go m' ls'
         else
           go m ls
    livesBeforeMap = Map.fromList $
      map (\lbl ->
              (lbl, fromMaybe Set.empty (Map.lookup lbl inits)))
      (map fst blocks ++ Map.keys inits)
    initWorkList = concat [Set.toList $ lookupMap' l preds | l <- Map.keys inits]
  in
    go livesBeforeMap initWorkList

lookupMap' :: (HasCallStack, Ord k, Show k) => k -> Map k v -> v
lookupMap' k m = case Map.lookup k m of
  Just v -> v
  Nothing -> throw $ MyException $ "key not found in map: " ++ show k ++ ", keys: " ++ show (Map.keys m) ++ "backtrace:\n" ++ prettyCallStack callStack

uncoverLives :: Asm -> PassM Asm
uncoverLives (AsmProgram info instrs) = 
  let blocks = splitBlocks instrs
      inits = Map.fromList [("conclusion", Set.singleton (ArgReg A1))]
      livesAfters = analyzeLiveness' blocks inits
  in pure $ AsmProgram (info { aiLivesMap = Just (Map.fromList livesAfters) }) instrs
