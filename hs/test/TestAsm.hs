{-# LANGUAGE MultilineStrings #-}
module TestAsm
  ( specAsm
  ) where

import Test.Hspec

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.Asm.Types
import Lang.Eoc.Asm.Liveness
import Lang.Eoc.Asm.Interferences (buildInterferences')

specAsm :: Spec
specAsm = do
  describe "Test asm passes" $ do
    testSplitBlocks
    testLiveness
    testInterferences

exampleBlocks :: [(String, [Instr])]
exampleBlocks =
  [ ("start", start)
  , ("block0", block0)
  , ("block2", block2)
  , ("block1", block1)
  ]
  where
    start  = [ Ilabel "start" $ Pmv (ArgVar "sum0") (ArgImm 0)
             , Pmv (ArgVar "i1") (ArgImm 5)
             , Ibranch "block0"
             ]
    block0 = [ Ilabel "block0" $ Pmv (ArgVar "tmp0") (ArgVar "i1")
             , IcondBranch (Blt (ArgReg X0) (ArgVar "tmp0")) "block2"
             , Ibranch "block1"
             ]
    block2 = [ Ilabel "block2" $ Pmv (ArgVar "tmp2") (ArgVar "i1")
             , Pmv (ArgVar "tmp1") (ArgVar "sum0")
             , Iadd (ArgVar "sum0") (ArgVar "tmp1") (ArgVar "tmp2")
             , Pmv (ArgVar "tmp3") (ArgVar "i1")
             , Iaddi (ArgVar "i1") (ArgVar "tmp3") (ArgImm (-1))
             , Ibranch "block0"
             ]
    block1 = [ Ilabel "block1" $ Pmv (ArgReg A1) (ArgVar "sum0")
             , Ibranch "conclusion"
             ]

testLiveness :: Spec
testLiveness = do
  it "does liveness scan" $ do
    let res = analyzeLiveness' exampleBlocks
          $ Map.singleton "conclusion" $ Set.singleton (ArgReg A1)
    lookup "start" res `shouldBe` Just (Set.fromList [ArgVar "i1", ArgVar "sum0"])
    lookup "block0" res `shouldBe` Just (Set.fromList [ArgVar "i1", ArgVar "sum0"])
    lookup "block2" res `shouldBe` Just (Set.fromList [ArgVar "i1", ArgVar "sum0"])
    lookup "block1" res `shouldBe` Just (Set.singleton (ArgReg A1))

testInterferences :: Spec
testInterferences = do
  it "builds interferences" $ do
    let livesMap = Map.fromList $ analyzeLiveness' exampleBlocks
          $ Map.singleton "conclusion" $ Set.singleton (ArgReg A1)
        g = buildInterferences' exampleBlocks livesMap
    Set.fromList (Map.keys g) `shouldBe` Set.fromList (map ArgVar ["i1", "sum0", "tmp0", "tmp1", "tmp2", "tmp3"]) -- check that all variables appear in the graph
    Map.lookup (ArgVar "i1") g `shouldBe` Just (Set.fromList $ map ArgVar ["sum0", "tmp0", "tmp1", "tmp2"])
    Map.lookup (ArgVar "sum0") g `shouldBe` Just (Set.fromList $ map ArgVar ["i1", "tmp0", "tmp2", "tmp3"])
    Map.lookup (ArgVar "tmp0") g `shouldBe` Just (Set.fromList $ map ArgVar ["i1","sum0"])
    Map.lookup (ArgVar "tmp1") g `shouldBe` Just (Set.fromList $ map ArgVar ["i1","tmp2"])
    Map.lookup (ArgVar "tmp2") g `shouldBe` Just (Set.fromList $ map ArgVar ["i1","sum0","tmp1"])
    Map.lookup (ArgVar "tmp3") g `shouldBe` Just (Set.fromList [ArgVar "sum0"])

testSplitBlocks :: Spec
testSplitBlocks = do
  it "splits blocks correctly" $ do
    splitBlocks (concatMap snd exampleBlocks) `shouldBe` exampleBlocks
