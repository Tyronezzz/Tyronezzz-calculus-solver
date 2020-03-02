module Printer where

import Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.String as Render
import Expressions
import Laws
import Calculations

--  print Expression
instance Pretty Expression where
    pretty (Con num) = pretty num
    pretty (Var s) = pretty s
    pretty (Derivative v e) = flatAlt (align (cat [lparen <> hang 2 ( pretty v <> comma <> softline <> align ( pretty e)), rparen]))
                                        (cat [lparen <> nest 2 (pretty v <> comma <> softline <> align ( pretty e)), rparen])
    pretty (SinExpr uOp e) = flatAlt (align (cat [lparen <> hang 2 ( pretty uOp <> softline <> align ( pretty e)), rparen]))
                                        (cat [lparen <> nest 2 (pretty uOp  <> softline <> align ( pretty e)), rparen])
    pretty (BiExpr biOp e1 e2) = flatAlt (align (cat [lparen <> hang 2 ( pretty e1 <> softline <> align ( pretty biOp) <> softline <> align ( pretty e2)), rparen]))
                                        (cat [lparen <> nest 2 (pretty e1  <> softline <> align ( pretty biOp) <> softline <> align ( pretty e2)), rparen])


--  print Unary operator
instance Pretty UnaryOp where
    pretty Sin = pretty "sin"
    pretty Cos = pretty "cos"
    pretty Tan = pretty "tan"
    pretty Ln = pretty "ln"
    pretty Neg = pretty "-"

--  print Binary operator
instance Pretty BinaryOp where
    pretty Add = pretty "+"
    pretty Sub = pretty "-"
    pretty Mul = pretty "*"
    pretty Div = pretty "/"
    pretty Pow = pretty "^"
    pretty Log = pretty "log"



--print Step
instance Pretty Step where
    pretty (Step lName e) = 
        lbrace <> pretty lName <> rbrace <> line <> equals <> align (cat [lparen <> hang 2  ( pretty e), rparen]) <> line
                                        
--print Calculation                                       
instance Pretty Calculation where
    pretty (Calc e ss) = align (lparen <>  ( pretty e ) <> rparen) <> line <> align (sep (map pretty ss))
                                      
