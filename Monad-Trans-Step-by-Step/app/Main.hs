module Main where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

import Atom
import qualified WithoutM as WOM
import qualified WithM as WM

exampleExp = Plus (Lit 12) (App (Abs "x" (Var "x")) (Plus (Lit 4) (Lit 2)))

main :: IO ()
main = 
  do
    return (WOM.eval0 Map.empty exampleExp) >>= print
    WM.eval1 Map.empty exampleExp >>= print
    print . runIdentity . runExceptT $ WM.eval2 Map.empty exampleExp

    
