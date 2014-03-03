module Test where

import qualified Control.Monad.State as S
import Data
import Interpreter
import Library

testInterpreter :: IO State
testInterpreter = do
  (_, s) <- S.runStateT initLibrary newInterpreter 
  return s

testExec :: State -> String -> IO (Expr, State)
testExec s text = do
  let result = evalSource s text
  case result of
    Left err -> do
		putStrLn $ "Parse error: "++(show err)
		return (ExprNil, s)
    Right ioAction -> do
                    (expr, s') <- ioAction
                    putStr $ unlines $ map errMessage (errors s')
		    putStrLn $ "=> " ++ (show expr)
		    return (expr, s')

testExecFile :: State -> FilePath -> IO (Expr, State)
testExecFile s path = do
  source <- readFile path
  testExec s source


