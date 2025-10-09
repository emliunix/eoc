module Main (main) where

import Test.Hspec

import SexpTest (sexpTestSpec)
import Test2 (mySpec2)
import TestAsm (specAsm)

main :: IO ()
main = do
  hspec $ do
    sexpTestSpec
    mySpec2
    specAsm
