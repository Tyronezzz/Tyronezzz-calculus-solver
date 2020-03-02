module Substitutions where
      --(Subst, apply)

import Expressions
import Data.Maybe (fromJust)

-- (varName, expr)
type Subst = [(Expression, Expression)]

-- replace the vars in subst with vars in expr
apply :: Expression -> Subst -> Expression

apply (BiExpr op e1 e2) subst = if(binding subst (BiExpr op e1 e2) /= Nothing) then fromJust (binding subst (BiExpr op e1 e2))
                                else BiExpr op (if (binding subst e1) == Nothing then (apply e1 subst) else fromJust (binding subst e1)) (if (binding subst e2) == Nothing then (apply e2 subst) else fromJust (binding subst e2))


apply (SinExpr op e1) subst  = if(binding subst (SinExpr op e1) /= Nothing ) then fromJust (binding subst (SinExpr op e1))
                              else SinExpr op (if (binding subst e1) == Nothing then (apply e1 subst) else fromJust (binding subst e1))


apply (Derivative e1 e2) subst = if(binding subst (Derivative e1 e2) /= Nothing) then fromJust (binding subst e1 e2))
                                  else Derivative e1 (if (binding subst e2) == Nothing then (apply e2 subst) else fromJust (binding subst e2))

apply (Con num) subst = if (binding subst (Con num)) /= Nothing then fromJust (binding subst (Con num))
                        else Con num

apply (Var v) subst = if(binding subst (Var v) /= Nothing) then fromJust (binding subst (Var v)) 
                      else Var v



binding :: Subst -> Expression -> Maybe Expression
binding sub v =  lookup v sub

sub = [(Var "x",Con 2),(Var "y",Con 3)]
eee = BiExpr Add (Var "y") (Var "x")