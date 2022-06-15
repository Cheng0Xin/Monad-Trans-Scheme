{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal where

import qualified Data.Text as T  
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

data LispVal 
  = Atom T.Text
  | List [LispVal]
  | Number Integer 
  | String T.Text 
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool

instance (Eq LispVal) where
  Atom x == Atom y = x == y
  List xs == List ys = xs == ys
  Number x == Number y = x == y
  String x == String y = x == y  
  Bool x == Bool y = x == y
  -- Fun x == Fun y = fn x == fn y
  Nil == Nil = True
  _ == _ = False 

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal}

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a}