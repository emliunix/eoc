{-# LANGUAGE MultilineStrings #-}
module SexpTest
  ( sexpTestSpec
  ) where

import Test.Hspec
import Text.Trifecta

import Lang.Eoc.Sexp
import Lang.Eoc.Parser
import Lang.Eoc.Types
import Lang.Eoc.R.Types

-- | Test cases
testCases :: [String]
testCases =
  [ "42"
  , "-123"
  , "hello"
  , "#t"
  , "#f"
  , "'()"
  , "\"hello world\""
  , "(+ 1 2)"
  , "(list 1 2 3)"
  , "(if #t 1 0)"
  , "(define square (lambda (x) (* x x)))"
  , "(cons 1 '())"
  , """; this is a comment
     \&(+ 1 ; another comment
     \&   2)
     \&"""
  , "(nested (list (+ 1 2) \"string\" #f))"
  , "(let ([a 10] [b 12]) a)"
  ]
expectedResults :: [Sexp]
expectedResults =
  [ Integer 42
  , Integer (-123)
  , Symbol "hello"
  , Bool True
  , Bool False
  , Nil
  , String "hello world"
  , List [Symbol "+", Integer 1, Integer 2]
  , List [Symbol "list", Integer 1, Integer 2, Integer 3]
  , List [Symbol "if", Bool True, Integer 1, Integer 0]
  , List [Symbol "define", Symbol "square", List [Symbol "lambda", List [Symbol "x"], List [Symbol "*", Symbol "x", Symbol "x"]]]
  , List [Symbol "cons", Integer 1, Nil]
  , List [Symbol "+", Integer 1, Integer 2]
  , List [Symbol "nested", List [Symbol "list", List [Symbol "+", Integer 1, Integer 2], String "string", Bool False]]
  , List [Symbol "let", List [List [Symbol "a", Integer 10], List [Symbol "b", Integer 12]], Symbol "a"]
  ]

shouldBeSuccess :: (Show a, Eq a) => Result a -> a -> Expectation
shouldBeSuccess (Success val) expected = val `shouldBe` expected
shouldBeSuccess (Failure err) _ = expectationFailure $ "Expected success but got failure: " ++ show err

testParse :: (String, Sexp) -> Expectation
testParse (input, expected) = do
  parseString parseSexp mempty input `shouldBeSuccess` expected

sexpTestSpec :: Spec
sexpTestSpec = describe "S-expression Parser Tests" $ do
  it "parses all example sexps" $ do
    mapM_ testParse (zip testCases expectedResults)
  it "parses R" $ do
    let input = "(let ([x 10]) (if #t (+ x 1) (- x 1)))"
        result = parseRfromString input
        expectedR = RDefsExpProgram Info [] $
          Let "x" (Int_ 10)
          (If (Bool_ True)
            (Prim PrimPlus [Var "x", Int_ 1])
            (Prim PrimSub [Var "x", Int_ 1]))
    -- putStrLn $ "sexp: " ++ show (success (parseSexpFromString input))
    result `shouldBeSuccess` expectedR
  it "parses defines" $ do
    let input = """
          \&(define (add [a:Int] [b:Int]) : Int
          \&  (+ a b))
          \&(add 1 2)
          \&"""
        prog = parseRfromString input
        expected = RDefsExpProgram Info
          [Def DefInfo "add" [("a", TyInt), ("b", TyInt)] TyInt
            (Prim PrimPlus [Var "a", Var "b"])
          ]
          (Apply (Var "add") [Int_ 1, Int_ 2])
    prog `shouldBeSuccess` expected
  it "parses while" $ do
    let input ="""
           \&(let ([x 10])
           \&  (begin
           \&    (while (> x 0)
           \&      (set! x (- x 1)))
           \&    x))
          """
        prog = parseRfromString input
        expected = RDefsExpProgram Info
          []
          (Let "x" (Int_ 10)
            (Begin
              [ While (Prim PrimGt [Var "x", Int_ 0])
                  (SetBang "x" (Prim PrimSub [Var "x", Int_ 1]))
              ]
              (Var "x")))
    prog `shouldBeSuccess` expected
