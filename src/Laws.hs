module Laws where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Data.List (partition)



parserLaw :: Parser Law
parserLaw = do{
                lawName <- parserString;
                _ <- space *> char ':';
                eq <- space *> parserEquation;
                return (Law lawName eq)}

parserEquation = do{
                    exp1 <- space *> expr;
                    _ <- space *> char '=';
                    exp2 <- space *> expr;
                    return (exp1, exp2)}


sortLaws :: [Law] -> Expression -> [Law]
sortLaws laws expr = laws

-- sortLaws laws = simple ++ others ++ defns
--      where
--      (simple, nonsimple) = partition isSimple laws
--      (defns, others)     = partition isDefn nonsimple

-- partition p xs = (filter p xs, filter (not . p) xs)


-- laws = ["addition : (x, a+b)=(x, a)+(x, b)",
--         "rule : (x, a*b)=(x, a)*b+a*(x, b)",
--         "sin : (x, sin(a))=cos(a)*(x, a)",
--         "cos : (x, cos(a))=-sin(a)*(x, a)", 
--         "ln : (x, ln(a))=(1/a)*(x, a)",
--         "power : (x, a^b)=a^b * (x, b*ln(a))", 
--         "derivative_self : (x, x)=1"]

laws = [Law "addition" (Derivative (Var "x") (BiExpr Add (Var "a") (Var "b")),BiExpr Add (Derivative (Var "x") (Var "a")) (Derivative (Var "x") (Var "b"))),

        Law "minus" (Derivative (Var "x") (BiExpr Sub (Var "a") (Var "b")),BiExpr Sub (Derivative (Var "x") (Var "a")) (Derivative (Var "x") (Var "b"))),

        Law "mul" (Derivative (Var "x") (BiExpr Mul (Var "a") (Var "b")),BiExpr Add (BiExpr Mul (Derivative (Var "x") (Var "a")) (Var "b")) (BiExpr Mul (Var "a") (Derivative (Var "x") (Var "b")))),

        Law "ln" (Derivative (Var "x") (SinExpr Ln (Var "a")),BiExpr Mul (BiExpr Div (Con 1) (Var "a")) (Derivative (Var "x") (Var "a"))),

        Law "power" (Derivative (Var "x") (BiExpr Pow (Var "a") (Var "b")),BiExpr Mul (BiExpr Pow (Var "a") (Var "b")) (Derivative (Var "x") (BiExpr Mul (Var "b") (SinExpr Ln (Var "a"))))),

        Law "derivativeSelf" (Derivative (Var "x") (Var "x"),Con 1),
        
        Law "derivativeCon" (Derivative (Var "x") (Con _),Con 0)]



-- constants: (x, c) = 0 if c does not depend on x

