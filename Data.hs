module Data where

import qualified Control.Monad.State as S
import Data.List (intersperse)
import qualified Data.Map as M

data Frame =
  Frame { frameBindings :: [M.Map String Expr]  -- stack of scopes
        , frameName :: String -- for recursion
        }
  deriving (Show)

data State = State { callStack :: [Frame] }
           deriving (Show)

type Eval = S.StateT State IO

data Error = Error { errMessage :: String }
           deriving (Show)

newtype Function = Function { evalFunc :: ([Expr] -> Eval Expr) }

instance Eq Function where
  (==) _ _ = False

instance Show Function where
  show _ = "<function>"

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
  show expr = case expr of
    ExprString s -> "str:\""++s++"\""
    ExprSymbol s -> "sym:"++s
    ExprInt i -> "int:"++(show i)
    ExprFloat f -> "float:"++(show f)
    ExprList xs -> "(" ++(concat $ intersperse " " $ map show xs)++")"
    ExprMap m -> "{"++(concat $ intersperse ", " $ map show m)++"}"
    ExprFunc _ -> "<function>"
    ExprNil -> "nil"

exprToString expr = case expr of
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

symName :: Expr -> Maybe String
symName (ExprSymbol s) = Just s
symName _ = Nothing

isNumeric :: Expr -> Bool
isNumeric (ExprInt _) = True
isNumeric (ExprFloat _) = True
isNumeric _ = False

adaptNumPair :: (Expr, Expr) -> (Expr, Expr)
adaptNumPair p = case p of
  (ExprFloat f, ExprInt i) -> (ExprFloat f, ExprFloat (realToFrac i))
  (ExprInt i, ExprFloat f) -> (ExprFloat (realToFrac i), ExprFloat f)
  _ -> p
  
