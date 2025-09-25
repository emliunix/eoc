module Main (main) where

import Test.Hspec

import Control.Monad ( (>=>) )

import Lang.Eoc.Types
import Lang.Eoc.RVar
import Lang.Eoc.CVar
import Lang.Eoc.Asm

compile :: Exp -> IO Asm
compile exp = do
  putStrLn "---- Beginning compilation..."
  let prog = Program RVarInfo exp
  putStrLn $ "Input program:\n" ++ show prog
  let lowered = evalGensym $ (uniquify >=> removeComplexOperators) prog
  putStrLn $ "Lowered program:\n" ++ show lowered
  let cprog = explicateControl lowered
  putStrLn $ "C program:\n" ++ show cprog
  let asm = assignHomes . selectInstructions $ cprog
  putStrLn $ "Output assembly:\n" ++ show asm
  putStrLn "---- Compilation finished"
  return asm

main :: IO ()
main = hspec $ do
  mySpec
  describe "Sample Test" $ do
    it "should pass this test" $ do
      (1 + 1) `shouldBe` (2 :: Int)

testExample1 :: Exp
testExample1 = Prim PrimPlus [Let "x" (Let "y" (Int_ 1) (Prim PrimNeg [Var "y"])) (Var "x"), Int_ 2]

mySpec :: Spec
mySpec = describe "My Spec" $ do
  it "should run test passes" $ do
    compile testExample1 `shouldThrow` const @_ @MyException True
