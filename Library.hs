module Library where

import Prelude hiding (print, div)

import qualified Control.Monad.State as S
import Data.List (intersperse)
import Text.Printf (printf)

import Data
import Interpreter

numericBinFunc
	:: (Int -> Int -> Int)
	-> (Float -> Float -> Float)
	-> [Expr] 
	-> Interpreter Expr
numericBinFunc fi ff (ex : ey : _) = 
	let (ax, ay) = adaptNumPair (ex, ey)
	in case (ax, ay) of
		(ExprInt a, ExprInt b) ->
			return $ ExprInt (a `fi` b)
		(ExprFloat a, ExprFloat b) ->
			return $ ExprFloat (a `ff` b)
		_ -> do
			pushError (Error $ 
				"Invalid arguments type: "
				++(typeName ex)++" and "++(typeName ey))
			return ExprNil
numericBinFunc _ _ xs = do
	pushError (Error $ "Invalid argument count: "++(show $ length xs))
	return ExprNil

div :: [Expr] -> Interpreter Expr
div (ex : ey : _) = 
    case adaptNumPair (ex, ey) of
      (ExprInt a, ExprInt b) -> return $ ExprFloat (fromIntegral a / fromIntegral b)
      (ExprFloat a, ExprFloat b) -> return $ ExprFloat (a/b)
      _ -> do
	pushError (Error $ 
		   "Invalid arguments type: "
		   ++(typeName ex)++" and "++(typeName ey))
	return ExprNil
div xs = do
  pushError (Error $ "Invalid argument count: "++(show $ length xs))
  return ExprNil

libToString :: [Expr] -> Interpreter Expr
libToString [] = return $ ExprString ""
libToString xs = return $ ExprString $ concat $ map exprToString xs

libPrint :: [Expr] -> Interpreter Expr
libPrint xs = do
    let string = concat $ map exprToString xs
    S.lift $ putStrLn string
    return $ ExprString string

libGetline :: [Expr] -> Interpreter Expr
libGetline [] = do
  str <- S.lift $ getLine
  return (ExprString str)
libGetline (msg:_) = do
  S.lift $ putStr (exprToString msg)
  libGetline []

eq :: [Expr] -> Interpreter Expr
eq [] = return ExprNil
eq (x:[]) = return ExprNil
eq (x:xs) = 
    if (all (\y -> x == y) xs)
    then return (ExprSymbol "true")
    else return ExprNil
  

lib =	[ ("+", numericBinFunc (+) (+))
	, ("-", numericBinFunc (-) (-))
	, ("*", numericBinFunc (*) (*))
        , ("/", div)

        , ("=", eq)

        , ("->string", libToString)

        , ("print", libPrint)
        , ("getline", libGetline)
	]

initLibrary :: Interpreter ()
initLibrary = do
	S.forM_ lib $ \(name, func) ->
		define name (ExprFunc $ Function { getFunction = func })


