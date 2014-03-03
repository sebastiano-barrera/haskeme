module Interpreter where

import 			Prelude hiding (lookup)
import qualified        Control.Monad.State as S
import qualified        Data.Map as M
import                  Text.Parsec (parse)
import                  Text.Parsec.Error (ParseError)

import Data
import qualified Parser as P

newInterpreter :: State
newInterpreter = State { bindings = []
                       , errors = [] 
                       }

pushScope :: Interpreter ()
pushScope = S.modify $ \s ->
	let b = bindings s
	in s { bindings = M.empty : b }

popScope :: Interpreter (M.Map String Expr)
popScope = do
	b <- S.gets bindings
	if b == [] then return M.empty
	else do
		S.modify $ \s -> s { bindings = tail b }
		return (head b)

define :: String -> Expr -> Interpreter Expr
define name val = do
	b <- S.gets bindings
	S.when (b == []) pushScope
	b' <- S.gets bindings
	S.modify $ \s -> 
		s { bindings = (M.insert name val (head b')) : (tail b') }
	return val

lookup :: String -> Interpreter (Maybe Expr)
lookup name = do
	b <- S.gets bindings
        lookup' name b
lookup' :: String -> [M.Map String Expr] -> Interpreter (Maybe Expr)
lookup' name b =
    case b of
      [] -> return Nothing
      (scope:next) ->
	  case M.lookup name scope of
            Just value -> return (Just value)
            Nothing -> lookup' name next

pushError :: Error -> Interpreter ()
pushError err = S.modify (\s -> s { errors = err : (errors s) } )

buildFunction :: [String] -> [Expr] -> ([Expr] -> Interpreter Expr)
buildFunction argNames body = 
	\argValues -> do
		pushScope
		S.forM_ (zip argNames argValues) $ \(name, value) ->
			define name value
		ret <- evalPrognS body
		popScope
		return ret
		

evalListS :: [Expr] -> Interpreter Expr
evalListS [] = return ExprNil

evalListS (ExprSymbol "quote" : []) = return ExprNil
evalListS (ExprSymbol "quote" : x : _) = return x -- not evaluating

evalListS (ExprSymbol "define" : args) =
  case args of
    (ExprSymbol name : value : _) -> do
    	value' <- evalS value
        define name value'
    (ExprSymbol name' : []) -> do
        pushError (Error $ "Value not given for definition of symbol `"
                             ++name'++"'")
        return ExprNil
    (x : _) -> do
      pushError (Error $ "Can't define anything that's not a symbol ("
                           ++(show x)++")")
      return ExprNil
    [] -> do
      pushError (Error $ "Symbol and corresponding value not given")
      return ExprNil

evalListS (ExprSymbol "lambda" : ExprList args : body) =
    let argNames = map (getSymbol "") $ filter isSymbol args
        func = buildFunction argNames body
    in return (ExprFunc $ Function { getFunction = func })
evalListS (ExprSymbol "lambda" : _ ) = do
  pushError (Error $ "Invalid lambda form")
  return ExprNil

evalListS (ExprSymbol "if" : cond : eThen : eElse : _) = do
  truth <- evalS cond
  case truth of
    (ExprSymbol "true") -> evalS eThen
    _ -> evalS eElse
evalListS (ExprSymbol "if" : _ ) = do
  pushError (Error $ "Invalid if form")
  return ExprNil

evalListS (ExprSymbol "progn" : body) = evalPrognS body

evalListS (ExprSymbol funcName : args) = do
	value <- lookup funcName
	case value of
		Just (ExprFunc func) -> do
			let func' = getFunction func
			argValues <- S.mapM evalS args
                        func' argValues
		Just expr -> do
			pushError (Error $ "Not defined as a function: "++funcName)
			return ExprNil
		Nothing -> do
			pushError (Error $ "Not defined: "++funcName)
			return ExprNil
evalListS (x:xs) = do    
  pushError (Error $ "Don't know how to eval form: "++(show $ ExprList (x:xs)))
  return ExprNil

evalSymbolS sym = do
  value <- lookup sym
  case value of
    Just value' -> return value'
    Nothing -> do
      pushError (Error $ "Undefined symbol: " ++ sym)
      return ExprNil
                                                
evalS :: Expr -> Interpreter Expr
evalS expr = 
    case expr of
      ExprList xs -> evalListS xs
      ExprSymbol sym -> evalSymbolS sym
      _ -> return expr



eval :: State -> Expr -> IO (Expr, State)
eval s expr = S.runStateT (evalS expr) s

evalPrognS :: [Expr] -> Interpreter Expr
evalPrognS [] = return ExprNil
evalPrognS (x:[]) = evalS x
evalPrognS (x:xs) = evalS x >> evalPrognS xs

evalProgn :: State -> [Expr] -> IO (Expr, State)
evalProgn s exprs = S.runStateT (evalPrognS exprs) s




evalSource :: State -> String -> Either ParseError (IO (Expr, State))
evalSource s text = do
  let res = parse P.source "(input)" text
  exprs <- res
  return $ evalProgn s exprs
