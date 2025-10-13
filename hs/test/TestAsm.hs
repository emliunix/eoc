{-# LANGUAGE MultilineStrings #-}
module TestAsm
  ( specAsm
  ) where

import Test.Hspec

import Control.Monad.ST (runST)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.Asm.Types
import Lang.Eoc.Asm.Liveness
import Lang.Eoc.Asm.AllocateRegisters (dSatur)
import Lang.Eoc.R
import Lang.Eoc.Parser (parseRfromString')

import Control.Monad ( (>=>) )
import Lang.Eoc.C (explicateControl, placeBlocks, mergeBlocks)

import Test2 (compile, Passes(..))
import Data.Foldable (foldrM)

specAsm :: Spec
specAsm = do
  describe "Test asm passes" $ do
    testSplitBlocks
    testLiveness
  specDSatur

testLiveness :: Spec
testLiveness = do
  it "does liveness scan" $ do
    -- testBlock2 `shouldReturn` ()
    let res = analyzeLiveness' exampleBlocks
              $ Map.singleton "conclusion" $ Set.singleton (ArgReg A1)
    lookup "start" res `shouldBe` Just (Set.fromList [ArgVar "i1", ArgVar "sum0"])
    lookup "block0" res `shouldBe` Just (Set.fromList [ArgVar "i1", ArgVar "sum0"])
    lookup "block2" res `shouldBe` Just (Set.fromList [ArgVar "i1", ArgVar "sum0"])
    lookup "block1" res `shouldBe` Just (Set.singleton (ArgReg A1))
  where
    exampleBlocks =
      [ ("start", start)
      , ("block0", block0)
      , ("block2", block2)
      , ("block1", block1)
      ]
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
    -- testBlock2 :: IO ()
    -- testBlock2 = do
    --   _ <- foldrM
    --     (\instr acc -> do
    --         let res = liveBefore instr acc
    --         putStrLn $ "instr: " ++ show instr ++ "\nlives: " ++ show res
    --         return res)
    --     (Set.fromList [ArgVar "i1", ArgVar "sum0"])
    --     block2
    --   return ()

testSplitBlocks :: Spec
testSplitBlocks = do
  it "splits blocks correctly" $ do
    splitBlocks (concatMap snd exampleBlocks) `shouldBe` exampleBlocks
  where
    exampleBlocks =
      [ ("start", start)
      , ("block0", block0)
      , ("block2", block2)
      , ("block1", block1)
      ]
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

specDSatur :: Spec
specDSatur = describe "dSatur algorithm tests" $ do
  it "colors graph correctly" $ do
    let graph = Map.fromList
                [ ("a", Set.singleton "b")
                , ("b", Set.fromList $ map (: []) "acde")
                , ("c", Set.fromList $ map (: []) "bf")
                , ("d", Set.fromList $ map (: []) "bef" )
                , ("e", Set.fromList $ map (: []) "bd" )
                , ("f", Set.fromList $ map (: []) "cd" )
                ]
        initColors = Map.fromList [("a", -1), ("c", -1)]
    dSatur graph initColors Map.empty `shouldBe`
         Map.fromList
         [ ("a", -1)
         , ("b", 0)
         , ("c", -1)
         , ("d", 1)
         , ("e", 2)
         , ("f", 0)
         ]
