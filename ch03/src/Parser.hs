module Parser(parseExpr) where

import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

-- | The language definition
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"                              -- | Start of a comment block
  , Tok.commentEnd      = "-}"                              -- | End of a comment block
  , Tok.commentLine     = "--"                              -- | Start of a comment line
  , Tok.nestedComments  = True                              -- | We do have nexted comments
  , Tok.identStart      = letter                            -- | The identifier starts with a letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"           -- | The identifier is an alphanum or _
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"     -- | Op can start with one of the characters
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"     -- | Op can contain one of the characters
  , Tok.reservedNames   = []                                -- | No reserved names yet
  , Tok.reservedOpNames = []                                -- | No reserved op names, yet
  , Tok.caseSensitive   = True                              -- | We are case sensitive
  }

-- | The lexer defined by the language definition
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

-- | Convenience function that defines a parser for items inside parenthesis.
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Convenience function that defines a parser for reserved words.
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | A parser that returns a list of items based on the individual parser.
semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | Defines a prefix operator
prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

-- Prefix operators
table :: Ex.OperatorTable String () Identity Expr
table = [
    [
      prefixOp "succ" ESucc
    , prefixOp "pred" EPred
    , prefixOp "iszero" EIsZero
    ]
  ]

-- Constants
true, false, zero :: Parser Expr
true  = reserved "true"  >> return ETrue
false = reserved "false" >> return EFalse
zero  = reservedOp "0"   >> return EZero

-- if/then/else
ifthen :: Parser Expr
ifthen = do
    reserved "if"
    cond <- expr
    reservedOp "then"
    tr <- expr
    reserved "else"
    EIf cond tr <$> expr

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
      true
  <|> false
  <|> zero
  <|> ifthen
  <|> parens expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s
