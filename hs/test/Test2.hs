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

import Lang.Eoc.C (explicateControl, placeBlocks, mergeBlocks)
import Lang.Eoc.Asm
  ( selectInstructions
  , uncoverLives
  , buildInterferences
  , uncoverMoves
  , allocateRegisters
  , patchInstructions
  , preludeConclusion
  )


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
  specFun

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
  it "compiles while example" $ do
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


specFun :: Spec
specFun = describe "Function Tests" $ do
  it "compiles functions" $ do
    test example
    test example2
  it "compiles fib-while" $ do
    test example3
  where
    test = compile passes
    passes = InitialPass ("typeCheckPass", typeCheckPass)
      :> ("shrink", shrink)
      :> ("uniquify", uniquify)
      :> ("revealFunctions", revealFunctions)
      :> ("uncoverGet", uncoverGet)
      :> ("removeComplexOperands", removeComplexOperands)
      :> ("explicateControl", explicateControl)
      :> ("placeBlocks", placeBlocks)
      :> ("mergeBlocks", mergeBlocks)
      :> ("selectInstructions", selectInstructions)
      :> ("uncoverLives", uncoverLives)
      :> ("uncoverMoves", uncoverMoves)
      :> ("buildInterferences", buildInterferences)
      :> ("allocateRegisters", allocateRegisters)
      :> ("patchInstructions", patchInstructions)
      :> ("preludeConclusion", preludeConclusion)
    example = parseRfromString' """
    \&(define (double [x: Int]) : Int
    \&  (+ x x))
    \& 
    \&(let ([v 10])
    \&  (double v))
    \&"""
    example2 = parseRfromString' """
    \&(define (fib-go [n: Int] [a: Int] [b: Int]) : Int
    \&  (if (> n 0)
    \&      (fib-go (- n 1) b (+ a b))
    \&      a))
    \&(define (fib [x: Int]) : Int (fib-go x 0 1))
    \&(fib 10)
    \&"""
    example3 = parseRfromString' """
    \&(define (fib [x: Int]) : Int
    \&  (let ([a 0]
    \&        [b 1])
    \&    (begin
    \&      (while (> x 0)
    \&        (let ([temp a])
    \&          (begin
    \&            (set! x (- x 1))
    \&            (set! a b)
    \&            (set! b (+ temp b)))))
    \&      a)))
    \&(fib 10)
    \&"""
