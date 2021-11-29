module Expr where

import Control.Applicative
import Parser

--
-- The expression
--

data Expr
    = Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Lit Int
    deriving Show

int :: Parser Expr
int = do Lit <$> number

infixOp :: String -> (a -> a -> a) -> Parser(a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor =
      int
  <|> parens expr

expr :: Parser Expr
expr = term `chainl1` addop

run :: String -> Expr
run = runParser expr

eval :: Expr -> Int 
eval ex = case ex of
    Add a b -> eval a + eval b 
    Mul a b -> eval a * eval b 
    Sub a b -> eval a - eval b 
    Lit n -> n

