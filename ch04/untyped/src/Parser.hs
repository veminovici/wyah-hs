module Parser(parseExpr) where

import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Syntax

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = []
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = do EVar <$> identifier

number :: Parser Expr
number = do ELit . LInt . fromIntegral <$> natural

lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    args <- many1 identifier
    reservedOp "."
    body <- expr
    return $ foldr ELam body args

term :: Parser Expr
term =  parens expr
    <|> variable
    <|> number
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 EApp es)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
