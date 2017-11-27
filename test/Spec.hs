{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Control.Monad.Trans.Except
import Data.Functor.Identity
import Data.IORef
import Language.Micro.Lisp
import qualified Data.Text as T

evalTest :: T.Text -> T.Text -> SpecWith ()
evalTest input output =
    it ("Evaluates " ++ show input ++ " to " ++ show output) $
    case runIdentity (runExceptT (parseAndRun pureSif input)) of
      Left err -> expectationFailure err
      Right ok -> prettyE ok `shouldBe` output

evalTestSideEffs :: T.Text -> T.Text -> [T.Text] -> SpecWith ()
evalTestSideEffs input output expectedWritten =
    it ("Evaluates " ++ show input ++ " to " ++ show output ++ " with output") $
    do ref <- newIORef []
       let sif =
               SideEffIf
               { se_write = modifyIORef' ref . (:)
               }
       res <- runExceptT (parseAndRun sif input)
       case res of
         Left err -> expectationFailure err
         Right ok ->
             do prettyE ok `shouldBe` output
                written <- reverse <$> readIORef ref
                written `shouldBe` expectedWritten

main :: IO ()
main =
    hspec $
    do describe "Test cases from cardl/micro-lisp (side eff free)" $
           -- derived from
           -- https://github.com/carld/micro-lisp/blob/master/test.sh
           do evalTest "(car (quote (1 2 3 4)))" "1.0"
              evalTest "(cdr (quote (1 2 3 4)))" "(2.0 3.0 4.0)"
              evalTest "(cons (quote 1) (cons (quote 2) null))" "(1.0 2.0)"
              evalTest "((lambda (x) (cons x (cons (quote 1) null))) (quote 7))" "(7.0 1.0)"
              evalTest "(pair? (quote (1 2 3)))" "(quote t)"
              evalTest "(eq? (quote hello) (quote hello))" "(quote t)"
              evalTest "(eq? (quote hello) (quote world))" "null"
              -- bad test case?: evalTest "(pair? (cons (quote hello) (quote world)))" "(quote t)"
              evalTest "(pair? (quote hello))" "null"
              -- bad test case ?: evalTest "((1 (x) (cons x (quote 1))) 2)" "null"
              evalTest "1" "1.0" -- different from original spec. was: null
              evalTest "(cons (quote 1) (cons (quote 2) (cons (quote 3) (cons (quote 4) null))))" "(1.0 2.0 3.0 4.0)"
              evalTest "(quote (1 2 3 4))" "(1.0 2.0 3.0 4.0)"
              evalTest "(cons (quote 1) (cons (quote 2) null))" "(1.0 2.0)"
              evalTest "((lambda (x y) (cons y (cons x null))) (quote 67) (quote 89))" "(89.0 67.0)"
       describe "Test cases from cardl/micro-lisp (with side effs)" $
           -- derived from
           -- https://github.com/carld/micro-lisp/blob/master/test.sh
           do evalTestSideEffs
                  "((lambda (x) (write x)) (quote hello))"
                  "(quote t)"
                  ["hello"]
              evalTestSideEffs
                  "(write (quote (cons (quote 1) (quote 2))))"
                  "(quote t)"
                  ["(cons (quote 1.0) (quote 2.0))"]
              evalTestSideEffs
                  "(write (cons (quote (hello world)) null))"
                  "(quote t)"
                  ["((hello world))"]

       describe "More test cases" $
           do evalTest "(sym? (quote foo))" "(quote t)"
              evalTest "(num? 1.0)" "(quote t)"
              evalTest "(if true (quote foo) (quote bar))" "foo"
              evalTest "(if (eq? (quote hello) (quote hello)) (quote foo) (quote bar))" "foo"
              evalTest "(if (eq? (quote hello) (quote hello)) (quote foo) (cons (quote bar) (quote bar)))" "foo"
              evalTest "(if false (quote foo) (quote bar))" "bar"
              evalTest "(apply eq? ((quote foo) (quote foo)))" "(quote t)"
