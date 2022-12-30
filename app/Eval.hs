{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Control.Exception
import Data.Map as Map
import Data.Text as T
import LispVal
import Parser

data LispException
  = NumArgs Integer [LispVal]
  | LengthOfList T.Text Int
  | ExpectedList T.Text
  | TypeMismatch T.Text LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String
  | IOError T.Text

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list

showError :: LispException -> T.Text
showError err =
  case err of
    (IOError txt) -> T.concat [T.pack "Error reading file ", txt]
    (NumArgs int args) -> T.concat [T.pack "Error Number of Arguments, expected ", T.pack $ show int, T.pack " recieved args: ", unwordsList args]
    (LengthOfList txt int) -> T.concat [T.pack "Error Length of List in ", txt, T.pack " length: ", T.pack $ show int]
    (ExpectedList txt) -> T.concat [T.pack "Error Expected List In Function ", txt]
    (TypeMismatch txt val) -> T.concat [T.pack "Error Type Mismatch: ", txt, showVal val]
    (BadSpecialForm txt) -> T.concat [T.pack "Error Bad Special Form: ", txt]
    (NotFunction val) -> T.concat [T.pack "Error Not a Function ", showVal val]
    (UnboundVar txt) -> T.concat [T.pack "Error Unbound Variable: ", txt]
    (PError str) -> T.concat [T.pack "Parser Error, expression cannot evaluate: ", T.pack str]
    (Default val) -> T.concat [T.pack "Error, Danger will Robinson! Evaluation could not proceed:  ", showVal val]

safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- Control.Exception.try m
  case result of
    Left (eTop :: SomeException) ->
      case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing -> return $ Left (show eTop)
    Right val -> return $ Right val

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = return val
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b) = return $ Number i
eval (List []) = return Nil
eval Nil = return Nil
eval (List [Atom "write", rest]) = return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) = return . String . T.pack . show $ List rest
eval n@(Atom _) = getVar n
eval (List [Atom "if", pred, truExpr, flsExpr]) = do
  ifRes <- eval pred
  case ifRes of
    (Bool True) -> eval truExpr
    (Bool False) -> eval flsExpr
    _ -> throw $ BadSpecialForm "if"
eval (List [Atom "let", List pairs, expr]) = do
  env <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals <- mapM eval $ getOdd pairs
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
   in local (const env') $ evalBody expr
eval (List [Atom "lambda", List params, expr]) = do
  return $ Lambda (IFunc $ applyLambda expr params) <$> ask
eval (List (Atom "lambda" : _)) = throw $ BadSpecialForm "lambda"
eval (List ((:) x xs)) = do
  funVar <- eval x
  xVal <- mapM eval xs
  case funVar of
    (Fun (IFunc internalFn)) -> internalFn xVal
    (Lambda (IFunc internalFn) boundenv) -> local (const boundenv) $ internalFn xVal
    _ -> throw $ NotFunction funVar

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
  env <- ask
  argEval <- mapM eval args
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) params argEval) <> env
   in local (const env') $ eval expr

getEven :: [t] -> [t]
getEven [] = []
getEven (x : xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x : xs) = getEven xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  env <- ask
  local (const $ Map.insert var evalVal env) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  env <- ask
  let envFn = const $ Map.insert var evalVal env
   in local envFn $ evalBody $ List rest
evalBody x = eval x

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
    Just x -> return x
    Nothing -> throw $ UnboundVar atom

evalFile :: T.Text -> IO ()
evalFile fileExpr = runASTinEnv basicEnv (fileToEvalForm fileExpr) >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input =
  either
    (throw . PError . show)
    evalBody
    $ readExprFile input

runParseTest :: T.Text -> T.Text
runParseTest input =
  either
    (T.pack . show)
    (T.pack . show)
    $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action =
  runResourceT $
    runReaderT (unEval action) code
