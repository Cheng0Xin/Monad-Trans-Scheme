module WithoutM where

import qualified Data.Map as Map
import Data.Maybe

import Atom


eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env 
eval0 env (Plus x1 x2) = 
  let IntVal xv1 = eval0 env x1
      IntVal xv2 = eval0 env x2
  in IntVal $ xv1 + xv2
eval0 env (Abs name exp) = FunVal env name exp
eval0 env (App ex1 ex2) = 
  let val1 = eval0 env ex1
      val2 = eval0 env ex2
  in case val1 of 
    FunVal env' name body -> eval0 (Map.insert name val2 env') body