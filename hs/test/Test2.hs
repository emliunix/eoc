{-# LANGUAGE MultilineStrings #-}
module Test2
  ( mySpec2
  , mySpec3
  , compile
  , Passes(..)
  ) where

import Test.Hspec

import Lang.Eoc.Types
import Lang.Eoc.R
import Lang.Eoc.Parser (parseRfromString')

import Control.Monad ( (>=>) )
import Lang.Eoc.C (explicateControl, placeBlocks, mergeBlocks)

data Passes a b where
  InitialPass :: (Show a) => (String, a -> PassM b) -> Passes a b
  (:>) :: (Show a, Show b, Show c) => Passes a c -> (String, c -> PassM b) -> Passes a b

compile :: (Show a, Show b) => Passes a b -> a -> IO ()
compile passes prog = do
  putStrLn $ "input:\n" ++ show prog
  putStrLn "----"
  _ <- runAndPrint passes prog
  return ()
  where
    runAndPrint :: forall a b. (Show a, Show b) => Passes a b -> a -> IO b
    runAndPrint (InitialPass (desc, p)) prog = do
      let r' = execPass (p prog)
      putStrLn $ desc ++ ":\n" ++ show r'
      putStrLn $ "----"
      return r'
    runAndPrint (xs :> (desc, p)) prog = do
      r <- runAndPrint xs prog
      let r' = execPass (p r)
      putStrLn $ desc ++ ":\n" ++ show r'
      putStrLn $ "----"
      return r'

mySpec2 :: Spec
mySpec2 = do
  mySpecRco
  mySpec3
  mySpecRWhile
  specPlaceBlocks

mySpecRco :: Spec
mySpecRco = describe "Test rco" $ do
  it "compiles rco example" $ do
    compile passes example `shouldReturn` ()
  where
    passes = InitialPass ("shrink", shrink)
      :> ("uniquify", uniquify)
      :> ("uncoverGet", uncoverGet)
      :> ("removeComplexOperands", removeComplexOperands)
    example = parseRfromString' "(let ([x 10]) (begin (set! x (+ x 1)) x))"

mySpec3 :: Spec
mySpec3 = describe "Test explicate-control" $ do
  it "examples compile" $ do
    test example `shouldReturn` ()
    test example2 `shouldReturn` ()
    test example3 `shouldReturn` ()
    test example4 `shouldReturn` ()
  where
    passes = InitialPass ("shrink", shrink)
      :> ("uniquify", uniquify)
      :> ("uncoverGet", uncoverGet)
      :> ("removeComplexOperands", removeComplexOperands)
      :> ("explicateControl", explicateControl)
    test = compile passes
    example = parseRfromString' """
    \& (let ([y (read)])
    \&   (let ([x (if (== y 0) 40 777)])
    \&     (+ x 2)))
    \&"""
    example2 = parseRfromString' """
    \& (let ([x (read)])
    \&   (if (== x 0) 42 777))
    \&"""
    example3 = parseRfromString' """
    \& (if (and (== (read) 0) (== (read) 1))
    \&   0
    \&   42)
    \&"""
    example4 = parseRfromString' """
    \& (let ([y (if #t
    \&             (read)
    \&             (if (== (read) 0)
    \&               777
    \&               (let ([x (read)])
    \&                 (+ 1 x))))])
    \&     (+ y 2))
    \&"""

mySpecRWhile :: Spec
mySpecRWhile = describe "R_while" $ do
  it "example compiles" $ do
    test example5 `shouldReturn` ()
  where
    passes = InitialPass ("shrink", shrink)
      :> ("uniquify", uniquify)
      :> ("uncoverGet", uncoverGet)
      :> ("removeComplexOperands", removeComplexOperands)
      :> ("explicateControl", explicateControl)
      :> ("placeBlocks", placeBlocks)
      :> ("mergeBlocks", mergeBlocks)
    test = compile passes
    example5 = parseRfromString' """
    \&(let ([sum 0])
    \&  (let ([i 5])
    \&    (begin
    \&      (while (> i 0)
    \&        (begin
    \&          (set! sum (+ sum i))
    \&          (set! i (- i 1))))
    \&      sum)))
    \&"""

specPlaceBlocks :: Spec
specPlaceBlocks = describe "BlockPlacement" $ do
  it "examples compiles" $ do
    test example `shouldReturn` ()
    test example2 `shouldReturn` ()
  where
    passes = InitialPass ("shrink", shrink)
      :> ("uniquify", uniquify)
      :> ("uncoverGet", uncoverGet)
      :> ("removeComplexOperands", removeComplexOperands)
      :> ("explicateControl", explicateControl)
      :> ("placeBlocks", placeBlocks)
      :> ("mergeBlocks", mergeBlocks)
    test = compile passes
    example = parseRfromString' """
    \& (let ([y (read)])
    \&   (let ([x (if (== y 0) 40 777)])
    \&     (+ x 2)))
    \&"""
    example2 = parseRfromString' """
    \& (let ([y (read)])
    \&   (let ([x (if (== y 0) 40 777)])
    \&     (+ x 2)))
    \&"""
