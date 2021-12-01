{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty (ppexpr) where

import Syntax

import Prelude hiding ((<>))
import Text.PrettyPrint

class Pretty p where
    ppr :: Int -> p -> Doc

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

instance Pretty Name where
  ppr _ x = text x

instance Pretty Expr where
    ppr _ (EVar x)         = text x
    ppr _ (ELit (LInt a))  = text (show a)
    ppr _ (ELit (LBool b)) = text (show b)
    ppr p e@(EApp _ _)     = parensIf (p>0) (ppr p f <+> sep (map (ppr (p+1)) xs))
        where (f, xs) = viewApp e
    ppr p e@(ELam _ _)     = parensIf (p>0) $ char '\\' <> hsep vars <+> text "." <+> body
        where
            vars = map (ppr 0) (viewVars e)
            body = ppr (p+1) (viewBody e)

viewVars :: Expr -> [Name]
viewVars (ELam n a) = n : viewVars a
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (ELam _ a) = viewBody a
viewBody x = x

viewApp :: Expr -> (Expr, [Expr])
viewApp (EApp e1 e2) = go e1 [e2]
  where
    go (EApp a b) xs = go a (b : xs)
    go f xs = (f, xs)
viewApp _ = error "not application"

ppexpr :: Expr -> String
ppexpr = render . ppr 0
