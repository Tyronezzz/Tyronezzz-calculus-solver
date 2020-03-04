module Match (match)
where
import Expressions
import Substitutions (Subst)


-- if law and expr are match, they must have same op
-- law:   x + 4
-- input: 3 + 4
-- [] -- not match
-- [[]] -- const match

-- match the left hand side of law with the expression, if so, return a Subst
match :: Expression -> Expression -> [Subst]

match (Derivative s1 eqn) (Derivative s2 e) =  [ a++b |a <- match s1 s2, b <- match eqn e, compatible a b]

match (BiExpr op_eqn eqn_left eqn_right) (BiExpr op e1 e2) | op == op_eqn
  = if match eqn_left e1 == [] ||  match eqn_right e2 == [] then []
  else [subsLeft ++ subsRight | subsLeft <- match eqn_left e1, subsRight <- match eqn_right e2,  compatible subsLeft subsRight]
                                                           | otherwise    = []

match (SinExpr op_eqn eqn) (SinExpr op e) | op == op_eqn = match eqn e
                                          | otherwise    = []

match (Var v) e = [[(Var v, e )]]

match (Con n1) (Con n2) 
    | n1 == n2 = [[]]
    | n1 == 0 && n2 /= 0 = []
    | n1 == 1 && n2 /= 1 = []
    | otherwise = []

match _ _ = [] 


-- compatible :: Subst -> Subst -> Bool
-- compatible [] _ = True
-- compatible _ [] = True
-- compatible [(lf_e1, rt_e1)] [(lf_e2, rt_e2)] = if lf_e1 /= lf_e2 then True else rt_e1 == rt_e2



compatible :: Subst -> Subst -> Bool
compatible [] _ = True
compatible _ [] = True
compatible subst1 subst2 = anyFalse boolList
                                where boolList = compatibleHelper subst1 subst2

compatibleHelper :: Subst -> Subst -> [Bool]
compatibleHelper subst1 subst2 = [compatibleInside subst1_ele subst2_ele | subst1_ele <- subst1, subst2_ele <- subst2]


compatibleInside :: (Eq a) => (a, a) -> (a, a) -> Bool
-- compatibleInside [] _ = True
-- compatibleInside _ [] = True
compatibleInside (lf_e1, rt_e1) (lf_e2, rt_e2) = if lf_e1 /= lf_e2 then True else rt_e1 == rt_e2

anyFalse :: [Bool] -> Bool
anyFalse [] = True
anyFalse (x:xs) = if x /= True then False else anyFalse xs
