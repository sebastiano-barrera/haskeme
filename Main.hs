module Main where

import qualified Control.Monad.State as S
import System.Environment (getArgs)
import System.IO
import Text.Parsec (parse)

import Data
import Eval
import Library
import System.Console.Readline (readline, addHistory)
import qualified Parser as P 

repl' = do
  text <- S.lift $ readline "haskeme> "
  case text of
    Nothing -> return ()
    Just text' -> 
        do
          S.lift $ addHistory text'
          res <- evalSource text'
          S.lift $ putStrLn $ "=> " ++ (show res)
          repl'

repl = do
  state <- newStateWithLib
  S.runStateT repl' state
  return ()

runFile filename = do
  state <- newStateWithLib
  source <- readFile filename
  S.runStateT (evalSource source) state
  return ()

printUsage = do
  putStrLn "Usage:"
  putStrLn " $ haskeme <script.scm>"
  return ()

main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    (filename : _) -> runFile filename
    _ -> repl

