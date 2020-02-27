module Matchings (match)
where
import Expressions
import Substitutions (Subst)
import Utilities (parts)


-- if law and expr are match, they must have same op
match :: Expression -> Expression -> [Subst]

match (Derivative s1 eqn) (Derivative s2 e) = match eqn e

match (BiExpr op_eqn eqn_left eqn_right) (BiExpr op e1 e2) | op == op_eqn = [(eqn_left, e1), (eqn_right, e2)] 
                                                           | otherwise    = []


match (SinExpr op_eqn eqn) (SinExpr op e) | op == op_eqn = [(eqn, e)]
                                          | otherwise    = []

match (Var eqn_v) (Var v) = [(eqn_v, v)]

