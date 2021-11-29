{-# LANGUAGE LambdaCase #-}

module Parser
    (
      Parser
    , chainl1
    , number
    , parens
    , reserved
    , runParser
    ) where

import Data.Char
import Control.Monad
import Control.Applicative
import GHC.Base (MonadPlus, Alternative, Functor)

newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- | Running the parser on a given stream will consume the stream and return the result.
runParser :: Parser a -> String -> a
runParser p s =
    case parse p s of
        [(res, [])] -> res
        [(_, rs)] -> error "The parser didnt consume the whole stream"
        _ -> error "Parser error"

-- | Extracts a single character from the stream.
item :: Parser Char
item = Parser $ \case
    [] -> []
    (c : tail) -> [(c, tail)]

-- | Builds a parser which always returns a given value, without consumig the stream.
unit :: a -> Parser a
unit a = Parser $ \s ->
    [(a, s)]

-- | Compose one parser's result with a second parser.
bind :: Parser a -> (a -> Parser b) -> Parser b
bind m f = Parser $ \s ->
    let xs = parse m s in concatMap (\(a, s1) -> parse (f a) s1) xs

-- Parser is a functor
instance Functor Parser where
    fmap f (Parser a) = Parser $ \s ->
        [(f a, s1) | (a, s1) <- a s]

-- | Parser is an applicative
instance Applicative Parser where
    pure = unit
    (Parser cs1) <*> (Parser cs2) = Parser $ \s ->
        [ (f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

-- | Parser is a monad
instance Monad Parser where
    return = unit
    (>>=) = bind

-- | A parser which halts reading by returning the empty string.
failure :: Parser a
failure = Parser $ const []

-- | Combines the results of two parsers.
combine :: Parser a -> Parser a -> Parser a
combine a b = Parser $ \s -> 
    parse a s ++ parse b s

option :: Parser a -> Parser a -> Parser a
option a b = Parser $ \s ->
    case parse a s of
    [] -> parse b s
    res -> res

-- | Parser is an alternative monad
instance Alternative Parser where
    empty = failure
    (<|>) = option

-- | Parser is a monadplus monad
instance MonadPlus Parser where
    mzero = failure
    mplus = combine


-- (<|>) :: Parser a -> Parser a -> Parser a
-- (<|>) = option


-- | One or more. 
-- The result is some_v, which is a v concat with many_v.
-- some :: (Alternative f, Functor f) => f a -> f [a]
-- some v = some_v where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

-- | Zero or more.
-- The result is many_v which can be some_v or nothing.
-- many :: (Alternative f,Functor f) => f a -> f [a]
-- many v = many_v where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

-- | Returns a parser which consumes the head of the stream
-- only if the character matches a condition.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
    if p c
    then unit c
    else failure

-- | Parses one or more occurences of p, separated by op
-- and returns a value obtained by a recursing until failure
-- on the left hand side of the stream.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a 
p `chainl1` op = do { a <- p; rest a }
    where rest a = (do f <- op; b <- p; rest (f a b))
                    <|> return a

-- | Parse zero or more occurences of p, separated by p
-- and returns a value obtained by accumulating the values.
-- If there are no occurences a default value is returned.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

--
-- Higher level parsers
--

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

-- | Returns a parser which parses the stream only if the head is a given character.
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | A parser which reads a sequence of digits
natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

-- || Returns a parser which reads a given string
string :: String -> Parser String 
string [] = return []
string (c : cs) = do { char c; string cs; return (c: cs) }

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved = token . string

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int 
number = do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
    reserved "("
    n <- m
    reserved ")"
    return n

