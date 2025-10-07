{-# LANGUAGE MultilineStrings #-}
module Test2
  ( mySpec2
  , mySpec3
  ) where

import Test.Hspec

import Lang.Eoc.Types
import Lang.Eoc.R
import Lang.Eoc.Parser (parseRfromString')

import Control.Monad ( (>=>) )
import Lang.Eoc.C (explicateControl)

data Passes a b where
  InitialPass :: (Show a) => (String, a -> PassM b) -> Passes a b
  (:>) :: (Show a, Show b, Show c) => Passes a c -> (String, c -> PassM b) -> Passes a b

compile :: (Show a, Show b) => Passes a b -> a -> IO ()
compile passes prog = do
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
mySpec2 = describe "My Spec 2" $ runIO $ do
    putStrLn $ "example:\n" ++ show example
    compile passes example
  where
    passes = InitialPass ("uniquify", uniquify)
      :> ("shrink", shrink)
      :> ("uncoverGet", uncoverGet)
      :> ("removeComplexOperands", removeComplexOperands)
    example = parseRfromString' "(let ([x 10]) (begin (set! x (+ x 1)) x))"

mySpec3 :: Spec
mySpec3 = describe "Test explicate-control" $ runIO $ do
  testExample example
  testExample example2
  testExample example3
  testExample example4
  testExample example5
  where
    passes = InitialPass ("uniquify", uniquify)
      :> ("shrink", shrink)
      :> ("uncoverGet", uncoverGet)
      :> ("removeComplexOperands", removeComplexOperands)
      :> ("explicateControl", explicateControl)
    testExample example = do
      putStrLn $ "example:\n" ++ show example
      putStrLn "----"
      compile passes example
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
