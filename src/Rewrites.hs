module Rewrites where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Laws 
import Substitutions

-- rewrites eqn e returns a list of all expressions that can arise by matching some subexpression 
-- of e against the left-hand expression of eqn and replacing the subexpression with the
--  appropriate instance of the right-hand expression of eqn.


-- For Binary expr,  we can consider rewrites as three parts, apply laws on exp itself, or on left expr or right expr. 
rewrites :: Equation -> Expression -> [Expression]

rewrites eqn (Derivative e1 e2) = (helper eqn (Derivative e1 e2)) -- (helper eqn (Derivative e1 e2)) ++ [Derivative e1 a | a <- rewrites eqn e2]

rewrites eqn (BiExpr op e1 e2) = (helper eqn (BiExpr op e1 e2)) ++ [BiExpr op a e2 | a <- rewrites eqn e1] ++ [BiExpr op e1 a | a <- rewrites eqn e2]

rewrites eqn (SinExpr op e) = helper eqn (SinExpr op e)

rewrites eqn (Con c) = [(Con c)]
rewrites eqn (Var v) = [(Var v)]


-- apply laws on on expr and get the new expr 
helper :: Equation -> Expression -> [Expression]
helper (Equation el er) exp = [apply er subst | subst <- match el exp]


