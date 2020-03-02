module Match (match)
where
import Expressions
import Substitutions (Subst)



-- if law and expr are match, they must have same op
-- law:   x + 4
-- input: 3 + 4
-- [[]]
-- []

match :: Expression -> Expression -> [Subst]

match (Derivative s1 eqn) (Derivative s2 e) = match eqn e

match (BiExpr op_eqn eqn_left eqn_right) (BiExpr op e1 e2) | op == op_eqn
  = if match eqn_left e1 == [] ||  match eqn_right e2 == [] then []
  else [subsLeft ++ subsRight | subsLeft <- match eqn_left e1, subsRight <- match eqn_right e2,  compatible subsLeft subsRight]
                                                           | otherwise    = []

match (SinExpr op_eqn eqn) (SinExpr op e) | op == op_eqn = match eqn e
                                          | otherwise    = []

match (Var v) e = [[(Var v, e )]]
match (Con n1) (Con n2) = [[]]

match _ _ = [] 



compatible :: Subst -> Subst -> Bool
compatible [] _ = True
compatible _ [] = True
compatible [(lf_e1, rt_e1)] [(lf_e2, rt_e2)] = if lf_e1 /= lf_e2 then True else rt_e1 == rt_e2


-- [] -- not match
-- [[]] -- const match