module Printer where

import Data.Text.Prettyprint.Doc as Doc
import Data.Text.Prettyprint.Doc.Render.Text as Render
import Text.Megaparsec
import Text.Megaparsec.Char
import Expressions
import Laws
import Calculations

import Text.Pandoc.Builder
import Text.Pandoc
import Text.Pandoc.Writers
import Text.Pandoc.Definition
import Text.Pandoc.Options
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL

-- show the result on the terminal
showResult str = case parse expr "" str of
                    Left bundle -> putStr (errorBundlePretty bundle)
                    Right a -> do {
                                    print (pretty (calculate laws a));
                                    generatePdf (pretty (calculate laws a));
                    }

resultDoc pc = doc $ header 1 (text ( ( Render.renderStrict (layoutCompact( pretty "A Haskell Calculus Solver!" )))))
           <> para ((text ( ( Render.renderStrict (layoutPretty defaultLayoutOptions pc ))) ) )


-- generate a word file
generatePdf pc = do
    docx <- runIO (writeDocx def (resultDoc pc)) >>= handleError
    BL.writeFile "result.docx" docx

--  print Expression
instance Pretty Expression where
    
    -- pretty (Con num) = flatAlt ( ( lparen <> (pretty num) <> rparen))
    --                                     ( lparen <> (pretty num)<>rparen)
    -- pretty (Var s) = flatAlt ( ( lparen <>  (pretty s)<> rparen))
    --                                     ( lparen <> (pretty s)<> rparen)

    pretty (Con num) = flatAlt (pretty num)
                                        (pretty num)
    pretty (Var s) = flatAlt (pretty s)
                                        (pretty s)                                   

    pretty (Derivative v e) = flatAlt ( ( lparen <> ( pretty v <> comma  <>  ( pretty e)) <> rparen))
                                        ( lparen <> (pretty v <> comma  <>  ( pretty e))<> rparen)
    pretty (SinExpr uOp e) = flatAlt ( ( lparen <>  ( pretty uOp  <>  ( pretty e)) <> rparen))
                                        ( lparen <>  (pretty uOp   <>  ( pretty e)) <> rparen)
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
        equals <>lbrace <> pretty lName <> rbrace <> line   <>  (  ( pretty e)) <> line

--print Calculation                                       
instance Pretty Calculation where
    pretty (Calc e ss) = ( ( pretty e )) <> line <>  (sep (map pretty ss))
