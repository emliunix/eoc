module Main (main) where

import Test.Hspec

import SexpTest (sexpTestSpec)
import Test2 (mySpec2)

main :: IO ()
main = hspec $ do
  sexpTestSpec
  mySpec2
  describe "Sample Test" $ do
    it "should pass this test" $ do
      (1 + 1) `shouldBe` (2 :: Int)
