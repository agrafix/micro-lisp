module Main where

import Language.Micro.Lisp

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main =
    do args <- getArgs
       case args of
         [x] ->
             parseAndRunIO (T.pack x) >>= \r ->
             case r of
               Left err -> error err
               Right ok -> T.putStrLn (prettyE ok)
         _ -> error "Usage: ./micro-lisp EXPR"
