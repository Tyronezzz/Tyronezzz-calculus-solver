module Rewrites where

import Expressions
import Substitutions
import Match

-- rewrites (eqn_l, eqn_r) e returns a list of all expressions that can arise by matching some subexpression 
-- of e against the left-hand expression of (eqn_l, eqn_r) and replacing the subexpression with the
--  appropriate instance of the right-hand expression of (eqn_l, eqn_r).


-- For Binary expr,  we can consider rewrites as three parts, apply laws on exp itself, or on left expr or right expr. 
rewrites :: Equation -> Expression -> [Expression]

rewrites (eqn_l, eqn_r) (Derivative e1 (Con n)) = [Con 0] 

rewrites (eqn_l, eqn_r) (Derivative e1 e2) = (helper (eqn_l, eqn_r) (Derivative e1 e2)) -- (helper (eqn_l, eqn_r) (Derivative e1 e2)) ++ [Derivative e1 a | a <- rewrites (eqn_l, eqn_r) e2]

rewrites (eqn_l, eqn_r) (BiExpr op e1 e2) = (helper (eqn_l, eqn_r) (BiExpr op e1 e2)) ++ [BiExpr op a e2 | a <- rewrites (eqn_l, eqn_r) e1] ++ [BiExpr op e1 a | a <- rewrites (eqn_l, eqn_r) e2]

rewrites (eqn_l, eqn_r) (SinExpr op e) = helper (eqn_l, eqn_r) (SinExpr op e)

rewrites (_, _) (Con c) = [(Con c)]
rewrites (_, _) (Var v) = [(Var v)]


-- apply laws on on expr and get the new expr 
helper :: Equation -> Expression -> [Expression]
helper (el, er) exp = [apply er subst | subst <- Match.match el exp]
