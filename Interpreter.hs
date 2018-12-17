{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Interpreter where

import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import LoopLang.Abs
import LoopLang.Print (printTree)

-- | The monad for the interpreter.
--
--   We use 'Reader' for the signature 'Sig',
--   'State' for the environment 'Env',
--   and 'Except' for returning from a function with 'Val'.

type Id = Ident

type Eval = State Env

type Env = Map Id Integer

-- | Entry point.

interpret :: Program -> IO ()
interpret (Prg ss x) =
  -- Run the statements in the initial environment.
  print $ lookupEnv x $ execState (evalStms ss) Map.empty

-- | Execute statements from left to right.

evalStms :: [Stm] -> Eval ()
evalStms = mapM_ evalStm

-- | Execute a single statement.

evalStm :: Stm -> Eval ()
evalStm s0 = case s0 of
  SOp x y op c -> do
    i <- lookupVar y
    assignVar x $ evalOp op i c
  SLoop x ss -> do
    n <- lookupVar x
    iter n $ evalStms ss

iter :: Monad m => Integer -> m () -> m ()
iter n m
  | n > 0     = m >> iter (n-1) m
  | otherwise = return ()

lookupVar :: Id -> Eval Integer
lookupVar x = lookupEnv x <$> get

lookupEnv :: Id -> Env -> Integer
lookupEnv x env = Map.findWithDefault 0 x env
  -- where err = error $ "uninitialized variable: " ++ printTree x

assignVar :: Id -> Integer -> Eval ()
assignVar x i = modify $ Map.insert x i

evalOp :: Num a => Op -> a -> a -> a
evalOp OPlus  = (+)
evalOp OMinus = (-)
