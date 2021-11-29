module Eval where

import Syntax

import Data.Maybe
import Data.Functor

-- | Returns true if the expression is a number.
isNum :: Expr -> Bool
isNum EZero     = True
isNum (ESucc t) = isNum t
isNum _         = False

-- | Returns true if the expression is a value.
isVal :: Expr -> Bool
isVal ETrue       = True
isVal EFalse      = True
isVal t | isNum t = True
isVal _           = False

eval' :: Expr -> Maybe Expr
eval' x = case x of
    EIsZero EZero              -> Just ETrue
    EIsZero(ESucc t) | isNum t -> Just EFalse
    EIsZero t                  -> EIsZero <$> eval' t
    ESucc t                    -> ESucc <$> eval' t
    EPred EZero                -> Just EZero
    EPred (ESucc t) | isNum t  -> Just t
    EPred t                    -> EPred <$> eval' t
    EIf ETrue t _              -> Just t
    EIf EFalse _ t             -> Just t
    EIf t a b                  -> (\t' -> EIf t' a b) <$> eval' t
    _                          -> Nothing 

normalForm :: Expr -> Expr
normalForm x = maybe x normalForm (eval' x)

eval :: Expr -> Maybe Expr 
eval t = case normalForm t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is ”stuck”
