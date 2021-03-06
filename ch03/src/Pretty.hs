module Pretty (
  ppexpr
) where

import Syntax

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

parensIf ::  Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Expr where
  ppr _ EZero = PP.text "0"
  ppr _ ETrue = PP.text "true"
  ppr _ EFalse = PP.text "false"
  ppr p (ESucc a) = (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
  ppr p (EPred a) = (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
  ppr p (EIsZero a) = (parensIf (p > 0) $ PP.text "iszero" <+> ppr (p+1) a)
  ppr p (EIf a b c) =
        PP.text "if"   <+> ppr p a
    <+> PP.text "then" <+> ppr p b
    <+> PP.text "else" <+> ppr p c

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0
