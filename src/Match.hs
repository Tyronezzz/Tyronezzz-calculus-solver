module Match (match)
where
import Expressions
import Substitutions (Subst)



-- type Subst = [(Expression, Expression)]





-- if law and expr are match, they must have same op
match :: Expression -> Expression -> [Subst]
match (Derivative s1 eqn) (Derivative s2 e) = match eqn e

match (BiExpr op_eqn eqn_left eqn_right) (BiExpr op e1 e2) | op == op_eqn
  = [subsLeft ++ subsRight | substLeft <- match eqn_left e1, substRight <- match eqn_righ e2, compatible subsLeft subsRight]

                                                           | otherwise    = [[]]

-- law:   x + 4
-- input: 3 + 4
-- [[]]
-- []


match (SinExpr op_eqn eqn) (SinExpr op e) | op == op_eqn = [(eqn, e)]
                                          | otherwise    = []

match (Var eqn_v) (Var v) = [(Var eqn_v, Var v)]

match (Var v) _ = [[(Var v, _)]]


