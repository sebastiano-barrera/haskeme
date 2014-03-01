module Parser where

import Data
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Char hiding (spaces)

spaces = many space

exprList = do
  char '('
  xs <- many expr
  spaces >> char ')'
  return (ExprList xs)

exprInt = do
  intStr <- many1 digit
  return $ ExprInt $ (read intStr :: Int)

exprFloat = do
  left <- many1 digit
  char '.'
  right <- many1 digit
  return $ ExprFloat $ (read (left ++ "." ++ right) :: Float)

exprString = do
  char '"'
  str <- many $ noneOf "\""
  char '"'
  return $ ExprString str

exprSymbol = do
  symbol <- many1 (oneOf "+=_-*/" <|> letter <|> digit)
  return $ ExprSymbol symbol

exprQuote = do
  char '\''
  quoted <- expr
  return $ ExprList [ExprSymbol "quote", quoted]

expr :: GenParser Char () Expr
expr = spaces >> choice [ exprQuote
                        , exprList
                        , exprInt
                        , exprFloat
                        , exprString
                        , exprSymbol -- must be last
                        ]

source = many expr

