module Calculations where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Rewrites
import Laws 

data Step = Step LawName Expression deriving Show
data Calculation = Calc Expression [Step]  deriving Show
type LawName = String

i1 = Derivative (Var "x") (BiExpr Mul (Var "x") (Con 2))
-- i2 = Derivative (Var "x") (BiExpr Sub (BiExpr Mul (Con 2) (Var "x")) (BiExpr Pow (Var "x") (Con 3)))
-- i3 = Derivative (Var "z") (BiExpr Pow (Var "z") (Var "y"))
i4 = Derivative (Var "x") (SinExpr Sin (BiExpr Pow (Var "x") (Con 2)))
-- i5 = Derivative (Var "p") (BiExpr Pow (Var "q") (Var "l"))
-- i6 = Derivative (Var "p") (SinExpr Sin (Var "q"))
-- i7 = Derivative (Var "x") (BiExpr Add (BiExpr Add (BiExpr Add (BiExpr Add (BiExpr Mul (Con 3) (BiExpr Pow (Var "x") (Con 4))) (BiExpr Mul (Con 4) (BiExpr Pow (Var "x") (Con 3)))) (BiExpr Mul (Con 6) (BiExpr Pow (Var "x") (Con 2)))) (BiExpr Mul (Con 12) (Var "x"))) (Con 12))
-- i8 = Derivative (Var "x") (BiExpr Mul (SinExpr Sin (Var "x")) (BiExpr Pow (Var "x") (BiExpr Div (Con 1) (Con 2))))

-- (x, 2*x - x^3)
-- e.g. calculate laws i2


calculate :: [Law] -> Expression -> Calculation
calculate laws e = Calc e (manyStep rws e)
    where rws e = [Step name e_new | Law name eqn <- sortedlaws, e_new <- rewrites eqn e, e_new /= e]
          sortedlaws = sortLaws laws e

manyStep :: (Expression -> [Step]) -> Expression -> [Step] 
manyStep rws e = case steps of 
                   [] -> []
                   (o@(Step _ e) : _) -> o:manyStep rws e 
                   where steps = rws e