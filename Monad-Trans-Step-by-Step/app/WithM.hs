module WithM where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

import Atom


{- Simple Identity -}
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Monad m => Env -> Exp -> m Value 
eval1 env (Lit v) = return $ IntVal v
eval1 env (Var name) = return . fromJust . (Map.lookup name) $ env
eval1 env (Plus e1 e2) = 
  let intValuePlus :: Value -> Value -> Value
      intValuePlus (IntVal x1) (IntVal x2) = IntVal $ x1 + x2
  in intValuePlus <$> eval1 env e1 <*> eval1 env e2
eval1 env (Abs name exp) = return $ FunVal env name exp
eval1 env (App e1 e2) = 
  let merge :: Monad m => Value -> Value -> m Value
      merge ev1 ev2 = 
        case ev1 of 
          FunVal env0 name body -> eval1 (Map.insert name ev2 env0) body
  in eval1 env e1 >>= merge <*> eval1 env e2

    
{- With ExceptT -}
type Eval2 a = ExceptT String Identity a {- newtype ExceptT l m r = ExceptT { runExceptT :: m (Either l r) } -}

runEval2 :: Eval2 a -> Either String a
runEval2 a = runIdentity (runExceptT a)

eval2 :: Env -> Exp -> Eval2 Value 
eval2 env (Lit v) = return $ IntVal v
eval2 env (Var name) = return . fromJust . (Map.lookup name) $ env
eval2 env (Plus e1 e2) = 
  let intValuePlus :: Value -> Value -> Value
      intValuePlus (IntVal x1) (IntVal x2) = IntVal $ x1 + x2
  in intValuePlus <$> eval2 env e1 <*> eval2 env e2
eval2 env (Abs name exp) = return $ FunVal env name exp
eval2 env (App e1 e2) = 
  let merge :: Value -> Value -> Eval2 Value
      merge ev1 ev2 = case ev1 of FunVal env0 name body -> eval2 (Map.insert name ev2 env0) body
  in eval2 env e1 >>= \x -> eval2 env e2 >>= \y -> merge x y

eval2b :: Env -> Exp -> Eval2 Value 
eval2b env (Lit v) = return $ IntVal v
eval2b env (Var name) = 
  case (Map.lookup name env) of 
    Just v -> return v 
    Nothing  -> throwError "Not found"
eval2b env (Abs name exp) = return $ FunVal env name exp
eval2b env (Plus e1 e2) =
  let work :: Value -> Value -> Eval2 Value
      work (IntVal iev1) (IntVal iev2) = return $ IntVal (iev1 + iev2)
      work _ _ = throwError  "Type not match in plus"
  in eval2b env e1 >>= \x -> eval2b env e2 >>= \y -> work x y
eval2b env (App e1 e2) =
  let work :: Value -> Value -> Eval2 Value
      work (FunVal env0 name body) v = eval2b (Map.insert name v env0) body
      work _ _ = throwError "Type not match in function application"
  in eval2b env e1 >>= \x -> eval2b env e2 >>= \y -> work x y
