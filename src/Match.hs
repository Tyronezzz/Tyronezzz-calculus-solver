module Match (match)
where
import Expressions
import Substitutions (Subst)



-- type Subst = [(Expression, Expression)]





-- if law and expr are match, they must have same op
-- law:   x + 4
-- input: 3 + 4
-- [[]]
-- []

match :: Expression -> Expression -> [Subst]
match (Derivative s1 eqn) (Derivative s2 e) = match eqn e

match (BiExpr op_eqn eqn_left eqn_right) (BiExpr op e1 e2) | op == op_eqn
  = [subsLeft ++ subsRight | subsLeft <- match eqn_left e1, subsRight <- match eqn_right e2, compatible subsLeft subsRight]

                                                           | otherwise    = []



match (SinExpr op_eqn eqn) (SinExpr op e) | op == op_eqn = [(eqn, e)]
                                          | otherwise    = []


match (Var v) _ = [[(Var v, _)]]


