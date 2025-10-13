module TestMoves where

import Test.Hspec

import Data.Set (Set)
import qualified Data.Set as Set

import Lang.Eoc.Types
import Lang.Eoc.Asm.Moves
import Lang.Eoc.Asm.Types (emptyAsmInfo, Asm(..), AsmInfo(..), Arg(..), Instr(..))

specMoves :: Spec
specMoves = do
  describe "Test uncoverMoves" $ do
    it "uncoverMoves works" $ do
      let asm = AsmProgram emptyAsmInfo
                [ Ilabel "start" $
                  Pmv (ArgVar "a") (ArgVar "b")
                , Pmv (ArgVar "b") (ArgVar "c")
                , Iadd (ArgVar "sum0") (ArgVar "sum0") (ArgVar "a")
                , Pmv (ArgVar "c") (ArgVar "a")
                , Pmv (ArgVar "d") (ArgVar "e")
                ]
      let (AsmProgram info' _) = execPass $ uncoverMoves asm
      aiMoves info' `shouldBe` Just expectedMoves
  where
    expectedMoves =
      [ Set.fromList
        [ ArgVar "a"
        , ArgVar "b"
        , ArgVar "c"
        ]
      , Set.fromList [ ArgVar "d", ArgVar "e" ]
      ]
