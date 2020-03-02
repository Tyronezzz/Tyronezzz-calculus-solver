module Rewrites where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Laws 
import Substitutions
import Match

-- rewrites (eqn_l, eqn_r) e returns a list of all expressions that can arise by matching some subexpression 
-- of e against the left-hand expression of (eqn_l, eqn_r) and replacing the subexpression with the
--  appropriate instance of the right-hand expression of (eqn_l, eqn_r).

input = Derivative (Var "x") (BiExpr Add (Var "a") (Var "b"))
eqn = (Derivative (Var "x") (BiExpr Add (Var "a") (Var "b")),Derivative (Var "x") (BiExpr Add (Var "b") (Var "a")))


-- For Binary expr,  we can consider rewrites as three parts, apply laws on exp itself, or on left expr or right expr. 
rewrites :: Equation -> Expression -> [Expression]

rewrites (eqn_l, eqn_r) (Derivative e1 (Con n)) = [Con 0] 

rewrites (eqn_l, eqn_r) (Derivative e1 e2) = (helper (eqn_l, eqn_r) (Derivative e1 e2)) -- (helper (eqn_l, eqn_r) (Derivative e1 e2)) ++ [Derivative e1 a | a <- rewrites (eqn_l, eqn_r) e2]

rewrites (eqn_l, eqn_r) (BiExpr op e1 e2) = (helper (eqn_l, eqn_r) (BiExpr op e1 e2)) ++ [BiExpr op a e2 | a <- rewrites (eqn_l, eqn_r) e1] ++ [BiExpr op e1 a | a <- rewrites (eqn_l, eqn_r) e2]

rewrites (eqn_l, eqn_r) (SinExpr op e) = helper (eqn_l, eqn_r) (SinExpr op e)

rewrites (eqn_l, eqn_r) (Con c) = [(Con c)]
rewrites (eqn_l, eqn_r) (Var v) = [(Var v)]


-- apply laws on on expr and get the new expr 
helper :: Equation -> Expression -> [Expression]
helper (el, er) exp = [apply er subst | subst <- Match.match el exp]

equation = (law_left, law_right)
law_left =  BiExpr Add (Var "x") (Var "y")
law_right = BiExpr Add (Var "y") (Var "x")

e1 = BiExpr Add (Con 2) (Con 3)