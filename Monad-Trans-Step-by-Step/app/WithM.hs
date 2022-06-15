module WithM where

import Atom
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe

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
    Nothing -> throwError "Not found"
eval2b env (Abs name exp) = return $ FunVal env name exp
eval2b env (Plus e1 e2) =
  let work :: Value -> Value -> Eval2 Value
      work (IntVal iev1) (IntVal iev2) = return $ IntVal (iev1 + iev2)
      work _ _ = throwError "Type not match in plus"
   in eval2b env e1 >>= \x -> eval2b env e2 >>= \y -> work x y
eval2b env (App e1 e2) =
  let work :: Value -> Value -> Eval2 Value
      work (FunVal env0 name body) v = eval2b (Map.insert name v env0) body
      work _ _ = throwError "Type not match in function application"
   in eval2b env e1 >>= \x -> eval2b env e2 >>= \y -> work x y

type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env m = runIdentity . runExceptT . runReaderT m $ env

eval3 :: Exp -> Eval3 Value
eval3 (Lit v) = return $ IntVal v
eval3 (Var name) = 
  ReaderT $ \env -> 
    case Map.lookup name env of
      Just x -> return x
      Nothing -> throwError "Not found variable"
eval3 (Plus x1 x2) = 
  let plus_value :: Value -> Value -> Eval3 Value
      plus_value (IntVal v1) (IntVal v2) = return . IntVal $ v1 + v2
      plus_value _ _ = throwError "Not suitable for +"
  in eval3 x1 >>= \x -> eval3 x2 >>= \y -> plus_value x y
eval3 (Abs name exp) = ReaderT $ \env -> return $ FunVal env name exp
eval3 (App exp1 exp2) = 
  let caller = eval3 exp1 
      arg = eval3 exp2
  in caller >>= \f -> 
    case f of 
      IntVal _ -> throwError "Int type not support for app"
      FunVal e0 name rf -> arg >>= \x ->
        local (const (Map.insert name x e0)) (eval3 rf)


type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either  String  a, Integer)
runEval4 env st m = runIdentity (runStateT (runExceptT (runReaderT m env)) st)

-- TODO