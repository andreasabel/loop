-- | Interpreter for the LOOP language.

module Interpreter where

import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import LoopLang.Abs
import LoopLang.Print (printTree)

-- | Monad for the interpreter.
--   A statement modifies the environment.

type Eval = State Env

-- | The environment maps variable identifiers to integers.

type Env = Map Ident Integer

-- | Entry point.
--
--   Run the statements in the initial environment
--   and print the value of the return variable.

interpret :: Program -> IO ()
interpret (Prg ss x) =
  print $ lookupEnv x $ execState (evalStms ss) Map.empty

-- | Execute statements from left to right.

evalStms :: [Stm] -> Eval ()
evalStms = mapM_ evalStm

-- | Execute a single statement.

evalStm :: Stm -> Eval ()
evalStm s = case s of

  SOp x y op c -> do
    i <- lookupVar y
    assignVar x $ evalOp op i c

  SLoop x ss -> do
    n <- lookupVar x
    iter n $ evalStms ss

-- | Interpret an operator.

evalOp :: Num a => Op -> a -> a -> a
evalOp OPlus  = (+)
evalOp OMinus = (-)

-- | Iterate a monadic computation @n@ times.

iter :: Monad m => Integer -> m () -> m ()
iter n m
  | n > 0     = m >> iter (n-1) m
  | otherwise = return ()

-- * Environment helpers

lookupVar :: Ident -> Eval Integer
lookupVar x = lookupEnv x <$> get

lookupEnv :: Ident -> Env -> Integer
lookupEnv x env = Map.findWithDefault 0 x env

assignVar :: Ident -> Integer -> Eval ()
assignVar x i = modify $ Map.insert x i
