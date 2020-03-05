{-# OPTIONS_GHC -Wall #-}
module Laws where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions


parserLaw :: Parser Law
parserLaw = do{
                lawName <- parserString;
                _ <- space *> char ':';
                eq <- space *> parserEquation;
                return (Law lawName eq)}

parserEquation :: ParsecT Void String Identity (Expression, Expression)
parserEquation = do{
                    exp1 <- space *> expr;
                    _ <- space *> char '=';
                    exp2 <- space *> expr;
                    return (exp1, exp2)}


sortLaws :: [Law] -> Expression -> [Law]
sortLaws laws expr = laws


laws :: [Law]
laws = [Law "Add" (Derivative (Var "x") (BiExpr Add (Var "a") (Var "b")),BiExpr Add (Derivative (Var "x") (Var "a")) (Derivative (Var "x") (Var "b"))),

        Law "Minus" (Derivative (Var "x") (BiExpr Sub (Var "a") (Var "b")),BiExpr Sub (Derivative (Var "x") (Var "a")) (Derivative (Var "x") (Var "b"))),

        Law "Mul" (Derivative (Var "x") (BiExpr Mul (Var "a") (Var "b")),BiExpr Add (BiExpr Mul (Derivative (Var "x") (Var "a")) (Var "b")) (BiExpr Mul (Var "a") (Derivative (Var "x") (Var "b")))),

        Law "Sin" (Derivative (Var "x") (SinExpr Sin (Var "a")),(BiExpr Mul (SinExpr Cos (Var "a")) (Derivative (Var "x") (Var "a")))),

        Law "Cos" (Derivative (Var "x") (SinExpr Cos (Var "a")), (BiExpr Mul (SinExpr Neg (SinExpr Sin (Var "a"))) (Derivative (Var "x") (Var "a")))),

        Law "Ln" (Derivative (Var "x") (SinExpr Ln (Var "a")),BiExpr Mul (BiExpr Div (Con 1) (Var "a")) (Derivative (Var "x") (Var "a"))),

        Law "Power" (Derivative (Var "x") (BiExpr Pow (Var "a") (Var "b")),BiExpr Mul (BiExpr Pow (Var "a") (Var "b")) (Derivative (Var "x") (BiExpr Mul (Var "b") (SinExpr Ln (Var "a"))))),

        Law "DerivativeSelf" (Derivative (Var "x") (Var "x"),Con 1),
        
        Law "DerivativeNotSelf" (Derivative (Var "x") (Var "y"), Con 0),

        Law "ZeroMul" (BiExpr Mul (Var "x") (Con 0), Con 0),  
        Law "OneMul" (BiExpr Mul (Var "x") (Con 1), Var "x"),
        Law "ZeroMul.2" (BiExpr Mul (Con 0) (Var "x"), Con 0),
        Law "OneMul.2" (BiExpr Mul (Con 1) (Var "x") , Var "x"),

        Law "ZeroAdd" (BiExpr Add (Var "x") (Con 0), Var "x"), 
        Law "ZeroAdd.2" (BiExpr Add (Con 0) (Var "x"), Var "x")
        ]


-- laws = ["addition : (x, a+b)=(x, a)+(x, b)",
--         "rule : (x, a*b)=(x, a)*b+a*(x, b)",
--         "sin : (x, sin(a))=cos(a)*(x, a)",
--         "cos : (x, cos(a))=-sin(a)*(x, a)", 
--         "ln : (x, ln(a))=(1/a)*(x, a)",
--         "power : (x, a^b)=a^b * (x, b*ln(a))", 
--         "derivative_self : (x, x)=1"]