module Eval where

import 			Prelude hiding (lookup)
import qualified        Control.Monad.State as S
import qualified        Data.Map as M
import                  Data.Maybe (catMaybes)
import                  Text.Parsec (parse)
import                  Text.Parsec.Error (ParseError)
import                  Text.Printf (printf)

import Data
import qualified Parser as P
import qualified Data.Map as M
import Control.Monad

maybeRead :: (Read a) => String -> Maybe a
maybeRead str =
  case reads str of
    [] -> Nothing
    [(x, _)] -> Just x

strIndent :: Int -> String -> String
strIndent indent str =
  unlines
  $ map (\line -> (indent `replicate` ' ') ++ line)
  $ lines str

userChoice :: [(String, Eval a)] -> Eval a
userChoice choices = do
  input <- S.lift $ do
    zip [ 1 :: Int .. ] choices `forM_` \(num, (msg, _)) -> do
      printf "%4d) %s\n" num msg
    putStr "Your choice: "
    getLine
  case maybeRead input of
    Just i ->
      if (i > 0) && (i <= length choices)
      then snd (choices !! (i - 1))
      else userChoice'
    _ -> userChoice'
  where
    userChoice' = do
      S.lift $ putStrLn "Choice was invalid."
      userChoice choices
    
evalError :: Expr -> String -> Eval Expr
evalError form msg = do
  S.lift $ do
    putStrLn " === Evaluation error:"
    putStrLn (5 `strIndent` msg)
    putStrLn "   - While evaluating:"
    putStrLn $ 5 `strIndent` show form
  userChoice [ ("BAIL", S.lift (putStrLn " BAIL!") >> return ExprNil)
             , ("MODIFY", S.lift (putStrLn " MODIFY!") >> return ExprNil)
             ]

newFrame :: String -> Frame
newFrame name =
    Frame { frameBindings = [M.empty]
          , frameName = name }

newState :: State
newState = State { callStack = [newFrame ""] }

withNewFrame :: String -> Eval a -> Eval a
withNewFrame name action = do
  state <- S.get
  S.put $ state { callStack = (newFrame name) : (callStack state) }
  ret <- action
  S.put state
  return ret

withLocalScope :: Eval a -> Eval a
withLocalScope action = do
  state <- S.get
  let stack' = 
          case (callStack state) of
            [] -> []
            (frame:stack') ->
                let frame' = frame { frameBindings = M.empty : (frameBindings frame) }
                in frame' : stack'

  S.put $ state { callStack = stack' }
  ret <- action
  S.put state
  return ret

define :: String -> Expr -> Eval Expr
define name val = do
  (frame:stack') <- S.gets callStack
  let (h:t) = frameBindings frame
  S.modify $ \state -> 
      state { callStack = frame { frameBindings = (M.insert name val h) : t }
                          : stack' 
            }
  return val

lookup' :: String -> [M.Map String Expr] -> Eval (Maybe Expr)
lookup' _ [] = return Nothing
lookup' name (scope:next) =
    case M.lookup name scope of
      Nothing -> lookup' name next
      Just val -> return $ Just val

lookup :: String -> Eval (Maybe Expr)
lookup name = do
  stack <- S.gets callStack
  let scopes = concatMap frameBindings stack
  lookup' name scopes

mkFunction :: String -> [String] -> [Expr] -> Function
mkFunction name argNames body = 
  let func argValues = withNewFrame name $ do 
        S.forM_ (zip argNames argValues) $
          \(name, value) -> define name value

        s <- S.get
        S.lift $ putStrLn ("Intepreter state before:\n"++(show s))

        ret <- evalProgn body

        s <- S.get
        S.lift $ putStrLn ("Intepreter state after:\n"++(show s))
        return ret
    in Function { evalFunc = func }

evalList :: [Expr] -> Eval Expr
evalList [] = return ExprNil

evalList (ExprSymbol "quote" : []) = return ExprNil
evalList (ExprSymbol "quote" : x : _) = return x -- not evaluating

evalList form@(ExprSymbol "define" : args) =
    case args of
      (ExprSymbol name : value : _) -> 
          do
    	    value' <- eval value
            define name value'
      (ExprSymbol name : []) ->
          do
            evalError
              (ExprList form)
              ("Value not given for definition of symbol `" 
               ++ name ++ "'")
            return ExprNil
      _ ->
          do
            evalError (ExprList form) ("Invalid def form: "++(show form))
            return ExprNil

evalList form@(ExprSymbol "defun" : args) =
    case args of
      (ExprSymbol name : (ExprList funcArgs) : body) -> 
          let argNames = catMaybes $ map symName funcArgs
          in if (length argNames) == (length funcArgs)
             then define name (ExprFunc $ mkFunction name argNames body)
             else do
               evalError (ExprList form) ("Invalid defn form: arguments not all symbols")
               return ExprNil
      _ -> do
        evalError (ExprList form) ("Invalid defun form: "++(show form))
        return ExprNil

evalList (ExprSymbol "lambda" : ExprList args : body) =
    let argNames = catMaybes $ map symName args
        func = mkFunction "" argNames body
    in return (ExprFunc func)

evalList form@(ExprSymbol "lambda" : _ ) = do
  evalError (ExprList form) ("Invalid lambda form")
  return ExprNil

evalList (ExprSymbol "if" : cond : eThen : eElse : _) = do
  truth <- eval cond
  case truth of
    (ExprSymbol "true") -> eval eThen
    _ -> eval eElse
evalList form@(ExprSymbol "if" : _ ) = do
  evalError (ExprList form) ("Invalid if form")
  return ExprNil

evalList (ExprSymbol "progn" : body) = evalProgn body

evalList form@(funcExpr : args) = do
  value <- eval funcExpr
  case value of
    (ExprFunc func) ->
        do
          args' <- S.mapM eval args
          (evalFunc func) args'
    _ -> 
        do
	  evalError (ExprList form) ("Can't eval as function: "++(show funcExpr))
	  return ExprNil

evalSymbolS sym = do
  value <- lookup sym
  case value of
    Just value' -> return value'
    Nothing -> do
      evalError (ExprSymbol sym) ("Undefined symbol: " ++ sym)
      return ExprNil

eval :: Expr -> Eval Expr
eval expr = 
    case expr of
      ExprList xs -> evalList xs
      ExprSymbol sym -> evalSymbolS sym
      _ -> return expr

evalProgn :: [Expr] -> Eval Expr
evalProgn [] = return ExprNil
evalProgn exprs = fmap last $ S.sequence $ map eval exprs

evalSource :: String -> Eval Expr
evalSource src = do
  let result = parse P.source "(input)" src
  case result of
    Left parseError -> 
        do
          S.lift $ putStrLn $ "parse error: " ++ (show parseError)
          return ExprNil
    Right exprs -> evalProgn exprs

{-
scmExec :: State -> String -> IO (Expr, State)
scmExec s text = do
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

scmExecFile :: State -> FilePath -> IO (Expr, State)
scmExecFile s path = do
  source <- readFile path
  scmExec s source
-}
