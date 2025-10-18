module TestRegisterAllocation where

import Test.Hspec

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.Asm.Liveness
import Lang.Eoc.Asm.Interferences
import Lang.Eoc.Asm.Moves
import Lang.Eoc.Asm.AllocateRegisters 
import Lang.Eoc.Asm.PatchInstructions
import Lang.Eoc.Asm.Types

import Test2 (Passes(..), compile)

specRegisterAllocation :: Spec
specRegisterAllocation = do
  describe "Test register allocation" $ do
    test1
    testMoveBiasing
    testCompileE2E

test1 :: Spec
test1 = do
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

testMoveBiasing :: Spec
testMoveBiasing = do
  it "colors graph with move biasing" $
    -- the example in the book
    -- iter1: y,w,x,v uncolored, saturation: y: 1, w: 1
    --        y has move-related var t colored 0, and the color is available
    --        so color y with 0
    -- iter2: w,x,v uncolored, saturation: w: 2
    --        color w with 2
    -- iter3: x,v uncolored, saturation: x: 1, v: 1
    --        but x has move-related var y = 0, z = 1
    --        so color x with 0
    -- iter4: v uncolored, saturation: v: 1
    --        move-related x = 0
    --        so color v with 0
    let graph = Map.fromList
                [ (ArgReg A0, Set.singleton $ ArgVar "t")
                , (ArgVar "t", Set.fromList [ArgReg A0, ArgVar "z"])
                , (ArgVar "z", Set.fromList [ArgVar "t", ArgVar "y", ArgVar "w"])
                , (ArgVar "y", Set.fromList [ArgVar "z", ArgVar "w"])
                , (ArgVar "w", Set.fromList [ArgVar "z", ArgVar "y", ArgVar "x", ArgVar "v"])
                , (ArgVar "x", Set.fromList [ArgVar "w"])
                , (ArgVar "v", Set.fromList [ArgVar "w"])
                ]
        initColors = Map.fromList [(ArgReg A0, -1), (ArgVar "t", 0), (ArgVar "z", 1)]
        moves = Map.fromList
                [ (ArgVar "t", Set.singleton (ArgVar "y"))
                , (ArgVar "y", Set.fromList  [ArgVar "t", ArgVar "x"])
                , (ArgVar "x", Set.fromList  [ArgVar "y", ArgVar "z", ArgVar "v"])
                , (ArgVar "z", Set.fromList  [ArgVar "x"])
                , (ArgVar "v", Set.fromList  [ArgVar "x"])
                ]
    in do dSatur graph initColors moves `shouldBe`
            Map.fromList
            [ (ArgReg A0, -1)
            , (ArgVar "t", 0)
            , (ArgVar "z", 1)
            , (ArgVar "y", 0)
            , (ArgVar "w", 2)
            , (ArgVar "x", 0)
            , (ArgVar "v", 0)
            ]

testCompileE2E :: Spec
testCompileE2E = do
  it "compiles with register allocation e2e" $ do
    test example `shouldReturn` ()
  where
    passes = InitialPass ("uncoverMoves", uncoverMoves)
      :> ("uncoverLives", uncoverLives)
      :> ("buildInterferences", buildInterferences)
      :> ("allocateRegisters", allocateRegisters)
      :> ("patchInstructions", patchInstructions)
    test = compile passes
    example = AsmDefsProgram emptyAsmInfo
      [AsmDef (emptyAsmDefInfo "start" "conclusion") "test" instrs]
    instrs =
      [ Ilabel "start" $ Pmv (ArgVar "v") (ArgImm 1)
      , Pmv (ArgVar "w") (ArgImm 42)
      , Iadd (ArgVar "x") (ArgVar "v") (ArgImm 7)
      , Pmv (ArgVar "y") (ArgVar "x")
      , Iadd (ArgVar "z") (ArgVar "x") (ArgVar "w")
      , Ineg (ArgVar "t") (ArgVar "y")
      , Iadd (ArgReg A0) (ArgVar "z") (ArgVar "t")
      , Ibranch "conclusion"
      ]
