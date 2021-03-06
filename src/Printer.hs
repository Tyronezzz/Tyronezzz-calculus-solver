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
           <> para ((code ( ( Render.renderStrict (layoutPretty defaultLayoutOptions pc ))) ) )


-- generate a word file
generatePdf pc = do
    docx <- runIO (writeDocx def (resultDoc pc)) >>= handleError
    BL.writeFile "result.docx" docx
                                      

instance Pretty Expression where
    
    pretty (Con num) =  (pretty num)
                            
    pretty (Var s) =  (pretty s)
                                        
    pretty (Derivative v e) =  ( ( lparen <> ( pretty v <> comma  <>  ( pretty e)) <> rparen))
                                        
    pretty (SinExpr uOp e) =  ( (  ( pretty uOp  <>  ( pretty e)) ))
                                       
    pretty (BiExpr biOp e1 e2) =  ( ( lparen <>  ( pretty e1  <>  ( pretty biOp)  <>  ( pretty e2)) <> rparen))

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
        equals <>lbrace <> pretty lName <> rbrace <> hardline   <>  (  ( pretty e)) <> hardline

--print Calculation                                       
instance Pretty Calculation where
    pretty (Calc e ss) = ( ( pretty e )) <> hardline <>  (sep (map pretty ss))
