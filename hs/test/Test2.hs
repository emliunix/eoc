module Test2
  ( mySpec2
  ) where

import Test.Hspec

import Lang.Eoc.Types
import Lang.Eoc.R
import Lang.Eoc.Parser (parseRfromString')

import Control.Monad ( (>=>) )

data Passes a b where
  InitialPass :: (Show a) => (a -> PassM b) -> Passes a b
  (:>) :: (Show a, Show b, Show c) => Passes a c -> (c -> PassM b) -> Passes a b

compile :: (Show a, Show b) => Passes a b -> a -> IO ()
compile passes prog = do
  _ <- runAndPrint passes prog
  return ()
  where
    runAndPrint :: forall a b. (Show a, Show b) => Passes a b -> a -> IO b
    runAndPrint (InitialPass p) prog = do
      let r' = execPass (p prog)
      putStrLn $ "Result: \n" ++ show r'
      putStrLn $ "----"
      return r'
    runAndPrint (xs :> p) prog = do
      r <- runAndPrint xs prog
      let r' = execPass (p r)
      putStrLn $ "Result: \n" ++ show r'
      putStrLn $ "----"
      return r'

mySpec2 :: Spec
mySpec2 = describe "My Spec 2" $ runIO $ do
    putStrLn $ "example: " ++ show example
    compile passes example
  where
    passes = InitialPass uniquify
      :> shrink
      :> uncoverGet
      :> removeComplexOperands
    example = parseRfromString' "(let ([x 10]) (begin (set! x (+ x 1)) x))"
