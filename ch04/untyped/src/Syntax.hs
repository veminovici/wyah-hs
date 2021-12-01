module Syntax where

type Name = String

data Expr
    = EVar Name                 -- variable
    | ELit Lit                  -- literal
    | EApp Expr Expr            -- application
    | ELam Name Expr            -- abstraction
    deriving (Eq, Show)

data Lit
    = LInt Int
    | LBool Bool
    deriving (Eq, Show)
