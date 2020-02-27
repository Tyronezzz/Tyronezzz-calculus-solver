module Substitutions where
      -- (Subst, unitSub, combine, apply)

import Expressions
import Utilities (cp)
import Data.Maybe (fromJust)

-- (varName, expr)
type Subst = [(Expression, Expression)]

-- replace the vars in subst with vars in expr
apply :: Expression -> Subst -> Expression
apply eqn_right subst = 

apply (BiExpr op e1 e2) subst | binding subst (BiExpr op e1 e2) != Nothing  =  binding subst (BiExpr op e1 e2)       
                              | otherwise                                   =  BiExpr op (if (binding subst e1) == Nothing then (apply e1 subst) else (binding subst e1)) (if (binding subst e2) == Nothing then (apply e2 subst) else (binding subst e2))
                                    



binding :: Subst -> Expression -> Expression
binding sub v = fromJust (lookup v sub)