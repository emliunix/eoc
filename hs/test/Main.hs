module Main (main) where

import Test.Hspec

import SexpTest (sexpTestSpec)
import Test2 (mySpec2)
import TestMoves (specMoves)
import TestRegisterAllocation (specRegisterAllocation)
import TestAsm (specAsm)

main :: IO ()
main = do
  hspec $ do
    sexpTestSpec
    mySpec2
    specAsm
    specMoves
    specRegisterAllocation
