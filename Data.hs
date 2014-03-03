module Data where

import qualified Control.Monad.State as S
import Data.List (intersperse)
import qualified Data.Map as M

newtype Error = Error { errMessage :: String }
    deriving (Show)

data State = State { bindings :: [M.Map String Expr]
                   , errors :: [Error]
                   }
             deriving (Show)

newtype Function = Function
    { getFunction :: ([Expr] -> Interpreter Expr) }
instance Eq Function where
    (==) _ _ = False

data Expr = ExprString String
          | ExprSymbol String
          | ExprInt Int
          | ExprFloat Float
          | ExprList [Expr]
          | ExprMap [(Expr, Expr)]
          | ExprFunc Function
          | ExprNil
            deriving (Eq)

instance Show Expr where
    show expr = 
        case expr of
          ExprString s -> "str:\""++s++"\""
          ExprSymbol s -> "sym:"++s
          ExprInt i -> "int:"++(show i)
          ExprFloat f -> "float:"++(show f)
          ExprList xs -> "(" ++(concat $ intersperse " " $ map show xs)++")"
          ExprMap m -> "{"++(concat $ intersperse ", " $ map show m)++"}"
          ExprFunc _ -> "<function>"
          ExprNil -> "nil"

type Interpreter = S.StateT State IO

exprToString expr = 
    case expr of
      ExprString s -> s
      ExprSymbol s -> s
      ExprInt i -> show i
      ExprFloat f -> show f
      ExprList xs -> "(" ++(concat $ intersperse " " $ map exprToString xs)++")"
      ExprFunc _ -> "<function>"
      ExprMap _ -> "<map>"
      ExprNil -> "nil"

typeName :: Expr -> String
typeName expr = case expr of
	ExprString _ -> "string"
	ExprSymbol _ -> "symbol"
	ExprInt _ -> "int"
	ExprFloat _ -> "float"
	ExprList _ -> "list"
	ExprFunc _ -> "function"
        ExprMap _ -> "map"
	ExprNil -> "nil"

getSymbol :: String -> Expr -> String
getSymbol _ (ExprSymbol sym) = sym
getSymbol defaultVal _ = defaultVal

isSymbol :: Expr -> Bool
isSymbol (ExprSymbol _) = True
isSymbol _ = False

isNumeric :: Expr -> Bool
isNumeric (ExprInt _) = True
isNumeric (ExprFloat _) = True
isNumeric _ = False

adaptNumPair :: (Expr, Expr) -> (Expr, Expr)
adaptNumPair p@(ExprFloat _, ExprFloat _) = p 
adaptNumPair p@(ExprInt _, ExprInt _) = p
adaptNumPair (ExprFloat f, ExprInt i) = (ExprFloat f, ExprFloat (realToFrac i))
adaptNumPair (ExprInt i, ExprFloat f) = (ExprFloat (realToFrac i), ExprFloat f)

