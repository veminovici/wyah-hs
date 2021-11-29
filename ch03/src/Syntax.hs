module Syntax where

data Expr
    = EFalse
    | ETrue
    | EIf Expr Expr Expr
    | EZero
    | ESucc Expr
    | EPred Expr
    | EIsZero Expr
    deriving (Eq, Show)
