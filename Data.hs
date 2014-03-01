module Data where

import Data.List (intersperse)

data Expr = ExprString String
          | ExprSymbol String
          | ExprInt Int
          | ExprFloat Float
          | ExprList [Expr]
          | ExprNil
            deriving (Eq)

instance Show Expr where
    show expr = 
        case expr of
          ExprString s -> "\""++s++"\""
          ExprSymbol s -> s
          ExprInt i -> (show i)
          ExprFloat f -> (show f)
          ExprList xs -> "("
                         ++(concat $ intersperse " " $ map show xs)
                         ++")"
          ExprNil -> "nil"


