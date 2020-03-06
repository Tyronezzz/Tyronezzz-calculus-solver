{-# LANGUAGE OverloadedStrings #-}
module Printer where

import Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.String as Render
import Text.Megaparsec
import Text.Megaparsec.Char
import Expressions
import Laws
import Calculations

import Text.Pandoc.Builder
import Text.Pandoc
import qualified Data.ByteString.Lazy as BL


showResult str = case parse expr "" str of
                    Left bundle -> putStr (errorBundlePretty bundle)
                    Right a -> print (pretty (calculate laws a))

mydoc = doc $ header 1 (text "Hello!")
           <> para (emph (text ( pretty (calculate laws i4))) <> text ".")


ggFunc = do
    let letter = mydoc
    docx <- runIO (writeDocx def letter) >>= handleError
    BL.writeFile "letter.docx" docx

--  print Expression
instance Pretty Expression where
    -- pretty (Con num) = flatAlt ( (cat [lparen <> (pretty num), rparen]))
    --                                     (cat [lparen <> (pretty num), rparen])
    -- pretty (Var s) = flatAlt ( (cat [lparen <>  (pretty s), rparen]))
    --                                     (cat [lparen <> (pretty s), rparen])

    -- pretty (Derivative v e) = flatAlt ( (cat [lparen <> ( pretty v <> comma <> softline <>  ( pretty e)), rparen]))
    --                                     (cat [lparen <> (pretty v <> comma <> softline <>  ( pretty e)), rparen])
    -- pretty (SinExpr uOp e) = flatAlt ( (cat [lparen <>  ( pretty uOp <> softline <>  ( pretty e)), rparen]))
    --                                     (cat [lparen <>  (pretty uOp  <> softline <>  ( pretty e)), rparen])
    -- -- pretty (SinExpr uOp e) = lparen <> pretty uOp <> pretty e <> rparen
    -- pretty (BiExpr biOp e1 e2) = flatAlt ( (cat [lparen <>  ( pretty e1 <> softline <>  ( pretty biOp) <> softline <>  ( pretty e2)), rparen]))
    --                                     (cat [lparen <> (pretty e1  <> softline <>  ( pretty biOp) <> softline <>  ( pretty e2)), rparen])
    pretty (Con num) = flatAlt ( ( lparen <> (pretty num) <> rparen))
                                        ( lparen <> (pretty num)<>rparen)
    pretty (Var s) = flatAlt ( ( lparen <>  (pretty s)<> rparen))
                                        ( lparen <> (pretty s)<> rparen)

    pretty (Derivative v e) = flatAlt ( ( lparen <> ( pretty v <> comma  <>  ( pretty e)) <> rparen))
                                        ( lparen <> (pretty v <> comma  <>  ( pretty e))<> rparen)
    pretty (SinExpr uOp e) = flatAlt ( ( lparen <>  ( pretty uOp  <>  ( pretty e)) <> rparen))
                                        ( lparen <>  (pretty uOp   <>  ( pretty e)) <> rparen)
    -- pretty (SinExpr uOp e) = lparen <> pretty uOp <> pretty e <> rparen
    pretty (BiExpr biOp e1 e2) = flatAlt ( ( lparen <>  ( pretty e1  <>  ( pretty biOp)  <>  ( pretty e2)) <> rparen))
                                        (  lparen <> (pretty e1   <>  ( pretty biOp)  <>  ( pretty e2)) <> rparen)


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
    pretty Expressions.Div = pretty "/"
    pretty Pow = pretty "^"
    pretty Log = pretty "log"



--print Step
instance Pretty Step where
    pretty (Step lName e) = 
        lbrace <> pretty lName <> rbrace <> line  <> equals <>  (  ( pretty e)) <> line


--print Calculation                                       
instance Pretty Calculation where
    pretty (Calc e ss) = ( ( pretty e )) <> line <>  (sep (map pretty ss))
                                      
