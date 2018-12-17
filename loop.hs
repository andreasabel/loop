-- | Interpreter for Uwe Schöning's LOOP language.

import System.Environment (getArgs)
import System.Exit        (exitFailure)

import LoopLang.Abs       (Program)
import LoopLang.Par       (pProgram, myLexer)
import LoopLang.ErrM      (Err(Ok,Bad))

import Interpreter        (interpret)

-- | Main: read file passed by only command line argument,
--   parse as LOOP code and interpret.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= parse >>= interpret
    _      -> do
      putStrLn "Usage: loop <SourceFile>"
      exitFailure

-- | Parse file contents as LOOP program.

parse :: String -> IO Program
parse s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok  tree -> return tree
