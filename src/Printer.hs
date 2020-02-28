module Printer where

import Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.String as Render
import Expressions
import Laws

-- prettyHelper::Expression -> Doc ann
-- prettyHelper (Reference s) = pretty s
-- prettyHelper (Application a lst) = flatAlt (align (cat [lparen <> hang 2 ( pretty a <> softline <> align (sep (map pretty lst))), rparen]))
--                                       (cat [lparen <> nest 2 (pretty a <> softline <> align (sep (map pretty lst))), rparen])
instance Pretty Expression where
   pretty (Con num) = pretty num
   pretty (Var s) = pretty s
   pretty (Derivative v e) = flatAlt (align (cat [lparen <> hang 2 ( pretty v <> comma <> softline <> align ( pretty e)), rparen]))
                                      (cat [lparen <> nest 2 (pretty v <> comma <> softline <> align ( pretty e)), rparen])
--    pretty (SinExpr uOp e) = 


instance Pretty UnaryOp where
    pretty Sin = sins
    pretty Cos = pretty "cos"