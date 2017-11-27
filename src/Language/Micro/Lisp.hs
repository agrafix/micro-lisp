{-# LANGUAGE OverloadedStrings #-}
module Language.Micro.Lisp
    ( parseAndRun, prettyE
    )
where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V

-- First, a small inline parser combinator library:
-- For more info on this, see
-- https://www.athiemann.net/2016/05/27/parser-combinators.html

type ParseError = T.Text
newtype Parser a
    = Parser { runParser :: T.Text -> (T.Text, Either ParseError a) }

instance Functor Parser where
    fmap f (Parser parse) =
        Parser $ \txt ->
        let (rest, result) = parse txt
        in (rest, fmap f result)

instance Applicative Parser where
    pure val = Parser $ \txt -> (txt, Right val)
    (Parser funParser) <*> continue =
        Parser $ \txt ->
        let (rest, result) = funParser txt
        in case result of
            Left err -> (rest, Left err)
            Right f -> runParser (fmap f continue) rest

instance Alternative Parser where
    empty = Parser $ \txt -> (txt, Left "Parsing failed!")
    (Parser pa) <|> otherParser =
        Parser $ \txt ->
        case pa txt of
          full@(_, Right _) -> full
          _ -> runParser otherParser txt

instance Monad Parser where
    return = pure
    fail errMsg = Parser $ \txt -> (txt, Left $ T.pack errMsg)
    (Parser parse) >>= next =
        Parser $ \txt ->
        let (leftOver, res) = parse txt
        in case res of
             Left errMsg -> (leftOver, Left errMsg)
             Right val -> runParser (next val) leftOver

satisfy :: (Char -> Bool) -> Parser T.Text
satisfy f =
    Parser $ \txt ->
    let (matches, rest) = T.span f txt
    in (rest, Right matches)

satisfy1 :: (Char -> Bool) -> Parser T.Text
satisfy1 f =
    satisfy f >>= \res ->
    do when (T.null res) $ fail "satisfy1 didn't ready anything!"
       pure res

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile = void . satisfy

charSatisfy :: (Char -> Bool) -> Parser Char
charSatisfy cond =
    Parser $ \txt ->
    case T.uncons txt of
      Just (firstC, rest) | cond firstC -> (rest, Right firstC)
      _ -> (txt, Left $ T.pack "Failed to parse single character")

char :: Char -> Parser Char
char c = charSatisfy ((==) c)

numStarter :: Parser T.Text
numStarter =
    do optNeg <- optional (char '-')
       rest <- satisfy1 isDigit
       pure $ maybe rest (`T.cons` rest) optNeg

double :: Parser Double
double =
    do firstPart <- numStarter
       secondPart <-
           optional $
           do ch <- char '.'
              rest <- satisfy1 isDigit
              pure (ch `T.cons` rest)
       pure $ (read . T.unpack) (firstPart <> fromMaybe "" secondPart)

lexeme :: Parser a -> Parser a
lexeme x = skipWhile isSpace *> x <* skipWhile isSpace

newtype Lambda =
    Lambda (Env -> V.Vector Expr -> Either String Expr)

instance Show Lambda where
    show _ = "[LAMBDA]"

instance Eq Lambda where
    (==) _ _ = False

-- This is the AST:
data Expr
    = EList !(V.Vector Expr)
    | ESym !T.Text
    | ENum !Double
    | EFun !Lambda -- Internal use.
    deriving (Show, Eq)

-- Now we can define the parser:
parseSym :: Parser T.Text
parseSym =
    satisfy1 (\c -> not (isSpace c) && c /= ')' && c /= '(')

parseExpr :: Parser Expr
parseExpr =
    (EList <$> lexeme parseList)
    <|> (ENum <$> lexeme double)
    <|> (ESym <$> lexeme parseSym)

parseList :: Parser (V.Vector Expr)
parseList =
    char '(' *> (V.fromList <$> some parseExpr) <* char ')'

-- let's write an interpreter:
newtype Env =
    Env [(T.Text, Env -> V.Vector Expr -> Either String Expr)]
    -- this should be a map, but we avoid the dependency?

fun1 ::
    T.Text
    -> Bool -- evaluate args?
    -> (Env -> Expr -> Either String b)
    -> (T.Text, Env -> V.Vector Expr -> Either String b)
fun1 name evalArgs handler =
    ( name
    , \env args ->
          if V.length args /= 1
          then Left (show name ++ " only takes 1 argument, but got " ++ show args)
          else if evalArgs
                  then evalExpr (V.head args) env >>= handler env
                  else handler env (V.head args)
    )

fun2 ::
    T.Text
    -> Bool -- evaluate args?
    -> (Env -> Expr -> Expr -> Either String b)
    -> (T.Text, Env -> V.Vector Expr -> Either String b)
fun2 name evalArgs handler =
    ( name
    , \env args ->
          if V.length args /= 2
          then Left (show name ++ " takes 2 arguments, but got " ++ show args)
          else if evalArgs
                  then evalExpr (args V.! 0) env >>= \a1 ->
                       evalExpr (args V.! 1) env >>= handler env a1
                  else handler env (args V.! 0) (args V.! 1)
    )

trueE :: Expr
trueE = EList $ V.fromList [ESym "quote", ESym "t"]

falseE, nullE :: Expr
nullE = EList mempty
falseE = nullE

getNum :: Expr -> Maybe Double
getNum (ENum x) = Just x
getNum _ = Nothing

getSym :: Expr -> Maybe T.Text
getSym (ESym x) = Just x
getSym _ = Nothing

getList :: Expr -> Maybe (V.Vector Expr)
getList (EList x) = Just x
getList _ = Nothing

lambdaImpl :: Env -> Expr -> Expr -> Either String Expr
lambdaImpl env@(Env envVals) argList body =
    case argList of
      EList vec ->
          let argNames = V.mapMaybe getSym vec
              funBody callArgs =
                  if V.length argNames /= V.length callArgs
                  then Left ("Called lambda with wrong number of args. Expected " ++ show argNames ++ ", but got: " ++ show callArgs)
                  else do evaledArgs <- mapM (flip evalExpr env) callArgs
                          let envList =
                                  V.toList $ V.zip argNames $
                                  fmap (\e -> \_ _ -> Right e) evaledArgs
                              localEnv = Env $ envVals ++ envList
                          evalExpr body localEnv
          in Right $ EFun $ Lambda $ \_ args -> funBody args -- TODO: env handling wrong?
      _ -> Left "First argument of lambda must be a list of symbols."

initEnv :: Env
initEnv =
    Env
    [ ("null", \_ _ -> Right nullE)
    , ("true", \_ _ -> Right trueE)
    , ("false", \_ _ -> Right falseE)
    , fun1 "pair?" True $ \_ arg -> Right (if isJust (getList arg) then trueE else falseE)
    , fun1 "sym?" True $ \_ arg -> Right (if isJust (getSym arg) then trueE else falseE)
    , fun1 "num?" True $ \_ arg -> Right (if isJust (getNum arg) then trueE else falseE)
    , fun1 "quote" False $ \_ arg -> Right arg
    , fun1 "car" True $ \_ arg ->
            case arg of
              EList vec | not (V.null vec) -> Right (V.head vec)
              _ -> Left ("Can not call car on: " ++ show arg)
    , fun1 "cdr" True $ \_ arg ->
            case arg of
              EList vec | not (V.null vec) -> Right (EList $ V.tail vec)
              _ -> Left ("Can not call cdr on: " ++ show arg)
    , fun2 "cons" True $ \_ argH argT ->
            case argT of
              EList vec -> Right (EList $ V.cons argH vec)
              _ -> Left ("Second argument of cons must be list, is: " ++ show argT)
    , fun2 "eq?" True $ \_ l r -> Right (if l == r then trueE else falseE)
    , fun2 "lambda" False lambdaImpl
    ]

evalExpr :: Expr -> Env -> Either String Expr
evalExpr e env =
    case e of
      ESym sym -> apply sym env mempty
      ENum _ -> Right e
      EFun _ -> Right e
      EList vec
          | V.null vec -> Right $ EList vec
          | otherwise ->
                do let h = V.head vec
                   call <-
                       case h of
                         ESym _ -> pure h
                         EFun _ -> pure h
                         _ -> evalExpr h env
                   case call of
                     ESym sym -> apply sym env (V.tail vec)
                     EFun (Lambda go) -> go env (V.tail vec)
                     _ -> Left ("Expected a symbol or lambda, but got: " ++ show call)

apply :: T.Text -> Env -> V.Vector Expr -> Either String Expr
apply sym e@(Env env) args =
    case lookup sym env of
      Just body -> body e args
      Nothing -> Left ("Undefined symbol " ++ show sym)

-- parse and run combined
parseAndRun :: T.Text -> Either String Expr
parseAndRun t =
    case runParser parseExpr t of
     (_, Right e) -> evalExpr e initEnv
     (leftOver, Left err) -> Left ("Syntax Error: " ++ show err ++ " Leftover: " ++ show leftOver)

-- for pretty printing
prettyE :: Expr -> T.Text
prettyE e =
    case e of
      ESym s -> s
      ENum x -> T.pack (show x)
      EList v
          | e == nullE -> "null"
          | otherwise -> "(" <> T.intercalate " " (V.toList $ prettyE <$> v) <> ")"
      EFun _ -> "<lambda>"
