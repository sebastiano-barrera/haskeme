module Interpreter where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import Text.Parsec (parse)
import Text.Parsec.Error (ParseError)

import Data
import qualified Parser as P

newtype Error = Error { errMessage :: String }
    deriving (Show)

data State = State { bindings :: M.Map String Expr
                   , errors :: [Error]
                   }
           deriving (Show)

newInterpreter :: State
newInterpreter = State { bindings = M.empty
                       , errors = [] 
                       }

define :: String -> Expr -> S.State State Expr
define name val = do
  S.modify $ \s -> 
      s { bindings = M.insert name val (bindings s) }
  return ExprNil

pushError :: Error -> S.State State ()
pushError err = S.modify (\s -> s { errors = err : (errors s) } )

evalListS :: [Expr] -> S.State State Expr
evalListS [] = return ExprNil
evalListS (ExprSymbol "define" : args) =
  case args of
    (ExprSymbol name : value : _) -> 
        define name value
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

evalListS f = do    
  pushError (Error $ "Don't know how to eval form: "++(show f))
  return ExprNil


evalSymbolS sym = do
  b <- S.gets bindings
  case (M.lookup sym b) of
    Just value -> return value
    Nothing -> do
      pushError (Error $ "Undefined symbol: " ++ sym)
      return ExprNil
                                                
evalS :: Expr -> S.State State Expr
evalS expr = 
    case expr of
      ExprList xs -> evalListS xs
      ExprSymbol sym -> evalSymbolS sym
      _ -> return expr

eval :: State -> Expr -> (Expr, State)
eval s expr = S.runState (evalS expr) s

evalPrognS :: [Expr] -> S.State State Expr
evalPrognS [] = return ExprNil
evalPrognS (x:[]) = evalS x
evalPrognS (x:xs) = evalS x >> evalPrognS xs

evalProgn :: State -> [Expr] -> (Expr, State)
evalProgn s exprs = S.runState (evalPrognS exprs) s

evalSource :: State -> String -> Either ParseError (Expr, State) 
evalSource s text = do
  let res = parse P.source "(input)" text
  exprs <- res
  return $ evalProgn s exprs


