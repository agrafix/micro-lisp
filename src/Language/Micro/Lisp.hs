{-# LANGUAGE OverloadedStrings #-}
module Language.Micro.Lisp
    ( parseAndRun, parseAndRunIO, prettyE
    , SideEffIf(..), ioSif, pureSif
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Char (isDigit, isSpace)
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

newtype Lambda m =
    Lambda (Env m -> V.Vector (Expr m) -> ExceptT String m (Expr m))

instance Show (Lambda m) where
    show _ = "[LAMBDA]"

instance Eq (Lambda m) where
    (==) _ _ = False

-- This is the AST:
data Expr m
    = EList !(V.Vector (Expr m))
    | ESym !T.Text
    | ENum !Double
    | EFun !(Lambda m) -- Internal use.
    deriving (Show, Eq)

-- Now we can define the parser:
parseSym :: Parser T.Text
parseSym =
    satisfy1 (\c -> not (isSpace c) && c /= ')' && c /= '(')

parseExpr :: Parser (Expr m)
parseExpr =
    (EList <$> lexeme parseList)
    <|> (ENum <$> lexeme double)
    <|> (ESym <$> lexeme parseSym)

parseList :: Parser (V.Vector (Expr m))
parseList =
    char '(' *> (V.fromList <$> some parseExpr) <* char ')'

-- let's write an interpreter:
data SideEffIf m
    = SideEffIf
    { se_write :: T.Text -> m ()
    }

ioSif :: SideEffIf IO
ioSif = SideEffIf T.putStrLn

pureSif :: SideEffIf Identity
pureSif = SideEffIf (\_ -> pure ())

data Env m =
    Env
    { _e_vals :: [(T.Text, Env m -> V.Vector (Expr m) -> ExceptT String m (Expr m))]
      -- ^ this should be a map, but we avoid the dependency?
    , e_sideEffs :: SideEffIf m
    }

fun1 ::
    Monad m => T.Text
    -> Bool -- evaluate args?
    -> (Env m -> Expr m -> ExceptT String m b)
    -> (T.Text, Env m -> V.Vector (Expr m) -> ExceptT String m b)
fun1 name evalArgs handler =
    ( name
    , \env args ->
          if V.length args /= 1
          then throwE (show name ++ " only takes 1 argument, but got " ++ show args)
          else if evalArgs
                  then evalExpr (V.head args) env >>= handler env
                  else handler env (V.head args)
    )

fun2 ::
    Monad m => T.Text
    -> Bool -- evaluate args?
    -> (Env m -> Expr m -> Expr m -> ExceptT String m b)
    -> (T.Text, Env m -> V.Vector (Expr m) -> ExceptT String m b)
fun2 name evalArgs handler =
    ( name
    , \env args ->
          if V.length args /= 2
          then throwE (show name ++ " takes 2 arguments, but got " ++ show args)
          else if evalArgs
                  then evalExpr (args V.! 0) env >>= \a1 ->
                       evalExpr (args V.! 1) env >>= handler env a1
                  else handler env (args V.! 0) (args V.! 1)
    )

fun3 ::
    Monad m => T.Text
    -> Bool -- evaluate args?
    -> (Env m -> Expr m -> Expr m -> Expr m -> ExceptT String m b)
    -> (T.Text, Env m -> V.Vector (Expr m) -> ExceptT String m b)
fun3 name evalArgs handler =
    ( name
    , \env args ->
          if V.length args /= 3
          then throwE (show name ++ " takes 3 arguments, but got " ++ show args)
          else if evalArgs
                  then evalExpr (args V.! 0) env >>= \a1 ->
                       evalExpr (args V.! 1) env >>= \a2 ->
                       evalExpr (args V.! 2) env >>= handler env a1 a2
                  else handler env (args V.! 0) (args V.! 1) (args V.! 2)
    )

constF :: Monad m => Expr m -> Env m -> args -> ExceptT String m (Expr m)
constF e _ _ = pure e

trueE :: Expr m
trueE = EList $ V.fromList [ESym "quote", ESym "t"]

falseE, nullE :: Expr m
nullE = EList mempty
falseE = nullE

getNum :: Expr m -> Maybe Double
getNum (ENum x) = Just x
getNum _ = Nothing

getSym :: Expr m -> Maybe T.Text
getSym (ESym x) = Just x
getSym _ = Nothing

getList :: Expr m -> Maybe (V.Vector (Expr m))
getList (EList x) = Just x
getList _ = Nothing

lambdaImpl :: Monad m => Env m -> Expr m -> Expr m -> ExceptT String m (Expr m)
lambdaImpl env@(Env envVals sif) argList body =
    case argList of
      EList vec ->
          let argNames = V.mapMaybe getSym vec
              funBody callArgs =
                  if V.length argNames /= V.length callArgs
                  then throwE ("Called lambda with wrong number of args. Expected " ++ show argNames ++ ", but got: " ++ show callArgs)
                  else do evaledArgs <- mapM (flip evalExpr env) callArgs
                          let envList =
                                  V.toList $ V.zip argNames $ fmap constF evaledArgs
                              localEnv = Env (envVals ++ envList) sif
                          evalExpr body localEnv
          in pure $ EFun $ Lambda $ \_ args -> funBody args -- TODO: env handling wrong?
      _ -> throwE "First argument of lambda must be a list of symbols."

ifImpl :: Monad m => Env m -> Expr m -> Expr m -> Expr m -> ExceptT String m (Expr m)
ifImpl env cond trueB falseB =
    do r <- evalExpr cond env
       if r == falseE
          then evalExpr falseB env
          else evalExpr trueB env

applyImpl :: Monad m => Env m -> Expr m -> Expr m -> ExceptT String m (Expr m)
applyImpl env funCall funArgs =
    do evaledArgs <-
           case funArgs of
             EList vec -> pure vec
             _ ->
                 evalExpr funArgs env >>= \q ->
                 case q of
                   EList vec -> pure vec
                   r -> throwE ("Bad arguments for apply: " ++ show r)
       callFun env funCall evaledArgs

initEnv :: Monad m => SideEffIf m -> Env m
initEnv =
    Env
    [ ("null", constF nullE)
    , ("true", constF trueE)
    , ("false", constF falseE)
    , fun1 "pair?" True $ \_ arg -> pure (if isJust (getList arg) then trueE else falseE)
    , fun1 "sym?" True $ \_ arg -> pure (if isJust (getSym arg) then trueE else falseE)
    , fun1 "num?" True $ \_ arg -> pure (if isJust (getNum arg) then trueE else falseE)
    , fun1 "quote" False $ \_ arg -> pure arg
    , fun1 "write" True $ \env arg ->
            do lift $ (se_write (e_sideEffs env)) $ prettyE arg
               pure trueE
    , fun1 "car" True $ \_ arg ->
            case arg of
              EList vec | not (V.null vec) -> pure (V.head vec)
              _ -> throwE ("Can not call car on: " ++ show arg)
    , fun1 "cdr" True $ \_ arg ->
            case arg of
              EList vec | not (V.null vec) -> pure (EList $ V.tail vec)
              _ -> throwE ("Can not call cdr on: " ++ show arg)
    , fun2 "cons" True $ \_ argH argT ->
            case argT of
              EList vec -> pure (EList $ V.cons argH vec)
              _ -> throwE ("Second argument of cons must be list, is: " ++ show argT)
    , fun2 "eq?" True $ \_ l r -> pure (if l == r then trueE else falseE)
    , fun2 "lambda" False lambdaImpl
    , fun2 "apply" False applyImpl
    , fun3 "if" False ifImpl
    ]

evalExpr :: Monad m => Expr m -> Env m -> ExceptT String m (Expr m)
evalExpr e env =
    case e of
      ESym sym -> apply sym env mempty
      ENum _ -> pure e
      EFun _ -> pure e
      EList vec
          | V.null vec -> pure $ EList vec
          | otherwise -> callFun env (V.head vec) (V.tail vec)

callFun :: Monad m => Env m -> Expr m -> V.Vector (Expr m) -> ExceptT String m (Expr m)
callFun env fun args =
    do call <-
           case fun of
             ESym _ -> pure fun
             EFun _ -> pure fun
             _ -> evalExpr fun env
       case call of
         ESym sym -> apply sym env args
         EFun (Lambda go) -> go env args
         _ -> throwE ("Expected a symbol or lambda, but got: " ++ show call)

apply :: Monad m => T.Text -> Env m -> V.Vector (Expr m) -> ExceptT String m (Expr m)
apply sym e@(Env env _) args =
    case lookup sym env of
      Just body -> body e args
      Nothing -> throwE ("Undefined symbol " ++ show sym)

-- parse and run combined
parseAndRunIO :: T.Text -> IO (Either String (Expr IO))
parseAndRunIO t = runExceptT $ parseAndRun ioSif t

parseAndRun :: Monad m => SideEffIf m -> T.Text -> ExceptT String m (Expr m)
parseAndRun sif t =
    case runParser parseExpr t of
     (_, Right e) -> evalExpr e (initEnv sif)
     (leftOver, Left err) -> throwE ("Syntax Error: " ++ show err ++ " Leftover: " ++ show leftOver)

-- for pretty printing
prettyE :: Expr m -> T.Text
prettyE e =
    case e of
      ESym s -> s
      ENum x -> T.pack (show x)
      EList v
          | e == nullE -> "null"
          | otherwise -> "(" <> T.intercalate " " (V.toList $ prettyE <$> v) <> ")"
      EFun _ -> "<lambda>"
