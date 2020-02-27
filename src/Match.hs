module Matchings (match)
where
import Expressions
import Substitutions (Subst, unitSub, combine)
import Utilities (parts)


-- if law and expr are match, they must have same op
match :: Expression -> Expression -> [Subst]
match (BiExpr op e1 e2) (BiExpr op_eqn eqn_left eqn_right) | op == op_eqn = [(e1, eqn_left), (e2, eqn_right)] 