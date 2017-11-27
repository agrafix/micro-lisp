{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Language.Micro.Lisp
import qualified Data.Text as T

evalTest :: T.Text -> T.Text -> SpecWith ()
evalTest input output =
    it ("Evaluates " ++ show input ++ " to " ++ show output) $
    case parseAndRun input of
      Left err -> expectationFailure err
      Right ok -> prettyE ok `shouldBe` output

main :: IO ()
main =
    hspec $
    do describe "Test cases from cardl/micro-lisp" $
           -- derived from
           -- https://github.com/carld/micro-lisp/blob/master/test.sh
           do evalTest "(car (quote (1 2 3 4)))" "1.0"
              evalTest "(cdr (quote (1 2 3 4)))" "(2.0 3.0 4.0)"
              evalTest "(cons (quote 1) (cons (quote 2) null))" "(1.0 2.0)"
              evalTest "((lambda (x) (cons x (cons (quote 1) null))) (quote 7))" "(7.0 1.0)"
              evalTest "(pair? (quote (1 2 3)))" "(quote t)"
              evalTest "(eq? (quote hello) (quote hello))" "(quote t)"
              evalTest "(eq? (quote hello) (quote world))" "null"
              evalTest "(pair? (cons (quote hello) (quote world)))" "(quote t)"
              evalTest "(pair? (quote hello))" "null"
              evalTest "((1 (x) (cons x (quote 1))) 2)" "null"
              evalTest "1" "1.0" -- different from original spec. was: null
              evalTest "((lambda (x) (write x)) (quote hello))" "hello"
              evalTest "(write (quote (cons (quote 1) (quote 2))))" "(cons (quote 1) (quote 2))"
              evalTest "(cons (quote 1) (cons (quote 2) (cons (quote 3) (cons (quote 4) null))))" "(1.0 2.0 3.0 4.0)"
              evalTest "(quote (1 2 3 4))" "(1.0 2.0 3.0 4.0)"
              evalTest "(cons (quote 1) (cons (quote 2) null))" "(1.0 2.0)"
              evalTest "(write (cons (quote (hello world)) null))" "((hello world))"
              evalTest "((lambda (x y) (cons y (cons x null))) (quote 67) (quote 89))" "(89.0 67.0)"
       describe "More test cases" $
           do evalTest "(sym? (quote foo))" "(quote t)"
              evalTest "(num? 1.0)" "(quote t)"
