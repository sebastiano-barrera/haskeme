module Library
       ( initLibrary
       , newStateWithLib
       ) where

import Prelude hiding (print, div)

import qualified Control.Monad.State as S
import Data.List (intersperse)
import Text.Printf (printf)
import Text.Parsec (parse)

import qualified Parser as P
import Data
import Eval

simpleError :: String -> Eval Expr
simpleError msg = do
  S.lift $ putStrLn msg
  return ExprNil

numericBinFunc
	:: (Int -> Int -> Int)
	-> (Float -> Float -> Float)
	-> [Expr] 
	-> Eval Expr
numericBinFunc fi ff (ex : ey : _) = 
	let (ax, ay) = adaptNumPair (ex, ey)
	in case (ax, ay) of
		(ExprInt a, ExprInt b) ->
			return $ ExprInt (a `fi` b)
		(ExprFloat a, ExprFloat b) ->
			return $ ExprFloat (a `ff` b)
		_ -> do
		  simpleError 
                     ("Invalid arguments type: "
		      ++(typeName ex)++" and "++(typeName ey))
		  return ExprNil
numericBinFunc _ _ xs = do
	simpleError ("Invalid argument count: "++(show $ length xs))
	return ExprNil

div :: [Expr] -> Eval Expr
div (ex : ey : _) = 
    case adaptNumPair (ex, ey) of
      (ExprInt a, ExprInt b) -> return $ ExprFloat (fromIntegral a / fromIntegral b)
      (ExprFloat a, ExprFloat b) -> return $ ExprFloat (a/b)
      _ -> do
	simpleError ("Invalid arguments type: "
		          ++(typeName ex)++" and "++(typeName ey))
	return ExprNil
div xs = do
  simpleError ("Invalid argument count: "++(show $ length xs))
  return ExprNil

scmToString :: [Expr] -> Eval Expr
scmToString [] = return $ ExprString ""
scmToString xs = return $ ExprString $ concatMap exprToString xs

scmPrintLn :: [Expr] -> Eval Expr
scmPrintLn xs = do
    let string = concat $ map exprToString xs
    S.lift $ putStrLn string
    return $ ExprString string

scmPrint :: [Expr] -> Eval Expr
scmPrint xs = do
    let string = concat $ map exprToString xs
    S.lift $ putStr string
    return $ ExprString string

scmGetline :: [Expr] -> Eval Expr
scmGetline [] = do
  str <- S.lift $ getLine
  return (ExprString str)
scmGetline (msg:_) = do
  S.lift $ putStr (exprToString msg)
  scmGetline []

scmRead :: [Expr] -> Eval Expr
scmRead (ExprString prompt : _) = do
  S.lift $ putStr prompt
  scmRead []
scmRead _ = do
  text <- S.lift $ getLine
  case parse P.expr "(input)" text of
    Right expr -> return expr
    Left parseErr -> do
        simpleError (show parseErr)
        return ExprNil

eq :: [Expr] -> Eval Expr
eq [] = return ExprNil
eq (x:[]) = return ExprNil
eq (x:xs) = 
    if (all (\y -> x == y) xs)
    then return (ExprSymbol "true")
    else return ExprNil
  
scmEval :: [Expr] -> Eval Expr
scmEval (x : _) = do
  withLocalScope $ eval x

scmEval [] = return ExprNil

scmInspect :: [Expr] -> Eval Expr
scmInspect _ = do
  state <- S.get
  S.lift $ do
    putStrLn $ "State: " ++ (show state)
  return ExprNil

lib = [ ("eval",      scmEval)

      , ("+",         numericBinFunc (+) (+))
      , ("-",         numericBinFunc (-) (-))
      , ("*",         numericBinFunc (*) (*))
      , ("/",         div)

      , ("=",         eq)

      , ("->string",  scmToString)

      , ("print",     scmPrint)
      , ("println",   scmPrintLn)
      , ("getline",   scmGetline)
      , ("read",      scmRead)

      , ("inspect-state", scmInspect)
      ]

initLibrary :: Eval ()
initLibrary = do
  S.forM_ lib $ \(name, func) ->
    define name (ExprFunc $ Function { evalFunc = func })

newStateWithLib :: IO State
newStateWithLib = do
  (_, s) <- S.runStateT initLibrary newState
  return s

