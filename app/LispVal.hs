{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Text as T

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a} deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadIO)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  -- deriving (Eq)

data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

instance Show LispVal where
  show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String str) -> T.pack (Prelude.concat ["\"", T.unpack str, "\""])
    (Number num) -> T.pack $ show num
    (Bool True) -> T.pack "#t"
    (Bool False) -> T.pack "#f"
    Nil -> T.pack "Nil"
    (List contents) -> T.concat [T.pack "(", T.unwords $ showVal <$> contents, T.pack ")"]
    (Fun _) -> T.pack "(internal function)"
    (Lambda _ _) -> T.pack "(lambda function)"
