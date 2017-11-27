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
             go (T.pack x)
         [] ->
             do ctx <- T.getContents
                go ctx
         _ -> error "Usage: ./micro-lisp EXPR"

go :: T.Text -> IO ()
go inp =
    parseAndRunIO inp >>= \r ->
    case r of
      Left err -> error err
      Right ok -> T.putStrLn (prettyE ok)
