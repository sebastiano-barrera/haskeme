module Parser where

import Data

import qualified Data.Map as M
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
  optional (char ':')
  symbol <- many1 (oneOf "+=_-*/<>" <|> letter <|> digit)
  return $ ExprSymbol symbol

exprQuote = do
  char '\''
  quoted <- expr
  return $ ExprList [ExprSymbol "quote", quoted]

mapKVPair = do
  k <- expr
  v <- expr
  return (k,v)

exprMap = do
  char '{'
  kvPairs <- many mapKVPair
  char '}'
  return $ ExprMap kvPairs

expr :: GenParser Char () Expr
expr = do 
	spaces 
	ret <- choice [ exprQuote
                      , exprMap
		      , exprList
		      , try exprFloat
		      , exprInt
		      , exprString
                      , try (string "nil" >> return ExprNil)
                      , exprSymbol -- must be last
		      ]
	spaces
	return ret

source = many expr

