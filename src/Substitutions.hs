module Substitutions where
      -- (Subst, unitSub, combine, apply)

import Expressions
-- import Utilities (cp)
-- import Data.Maybe (fromJust)

-- (varName, expr)
type Subst = [(Expression, Expression)]

-- replace the vars in subst with vars in expr
apply :: Equation -> Subst -> Expression
apply  subst = 