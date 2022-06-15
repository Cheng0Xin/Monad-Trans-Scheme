module Atom where

import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show)

data Value = IntVal Integer
  | FunVal Env Name Exp
  deriving (Show)

type Env = Map.Map Name Value