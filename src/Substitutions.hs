module Substitutions
      (Subst, unitSub, combine, apply)
where
import Expressions
import Utilities (cp)
import Data.Maybe (fromJust)

type Subst = [(Expression, Expression)]

apply eqn_right subst = 