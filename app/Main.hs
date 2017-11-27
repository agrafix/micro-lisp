module Main where

import Language.Micro.Lisp

import Data.Maybe
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main =
    do args <- getArgs
       debug <- isJust . lookup "DEBUG" <$> getEnvironment
       case args of
         ["-f", fname] ->
             do ctx <- T.readFile fname
                go debug ctx
         [x] ->
             go debug (T.pack x)
         [] ->
             do ctx <- T.getContents
                go debug ctx
         _ -> error "Usage: ./micro-lisp [-f FILE|EXPR]"

go :: Bool -> T.Text -> IO ()
go debug inp =
    parseAndRunIO debug inp >>= \r ->
    case r of
      Left err -> error err
      Right ok -> T.putStrLn (prettyE ok)
