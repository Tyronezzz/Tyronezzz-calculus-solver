module Substitutions where
      -- (Subst, unitSub, combine, apply)

import Expressions
import Utilities (cp)
import Data.Maybe (fromJust)

-- (varName, expr)
type Subst = [(Expression, Expression)]

-- replace the vars in subst with vars in expr
apply :: Expression -> Subst -> Expression
-- apply eqn_right subst = 

apply (BiExpr op e1 e2) subst | binding subst (BiExpr op e1 e2) != Nothing  =  binding subst (BiExpr op e1 e2)       
                              | otherwise                                   =  BiExpr op (if (binding subst e1) == Nothing then (apply e1 subst) else (binding subst e1)) (if (binding subst e2) == Nothing then (apply e2 subst) else (binding subst e2))
                                    
apply (SinExpr op e1) subst | binding subst (SinExpr op e1) != Nothing = binding subst (SinExpr op e1)
                            | otherwise = SinExpr op (if (binding subst e1) == Nothing then (apply e1 subst) else (binding subst e1) )

apply (Derivative e1 e2) subst | binding subst (Derivative e1 e2) != Nothing = binding subst (Derivative e1 e2)
                               | otherwise Derivative e1 (if (binding subst e2) == Nothing then (apply e2 subst) else (binding subst e2))

apply (Con num) subst | binding subst (Con num) != Nothing = binding subst (Con num)
                      | otherwise Con num

apply (Var v) subst | binding subst (Var v) != Nothing = binding subst (Var v)
                    | otherwise Var v



binding :: Subst -> Expression -> Expression
binding sub v = fromJust (lookup v sub)