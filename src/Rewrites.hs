module Rewrites where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Substitutions
import Laws 


-- rewrites eqn e returns a list of all expressions that can arise by matching some subexpression 
-- of e against the left-hand expression of eqn and replacing the subexpression with the
--  appropriate instance of the right-hand expression of eqn.

-- 1.divide the expr to subexpression
-- 2.match subexpr to left hand eqn
-- 3.replace subexpr with right hand eqn


-- can we consider rewrites just do one step and then match????
-- Then it can be really simple

rewrites eqn e = apply e2 (match e e1) where (e1, e2) = eqn




-- rewrites :: Equation -> Expression -> [Expression]
-- rewrites eqn (Con n) = []
-- rewrites eqn (Var v) = []
-- rewrites eqn (Derivative v e) = map Derivative v (rewrites eqn e)
-- -- rewrites eqn (BiExpr biOp e1 e2) = 
-- -- rewrites eqn (SinExpr uOp e) = 


-- 1.divide the expr to subexpression, just get the smallest unit
-- e.g splits (1+2+3) => (Add, (Add, 1,2), 3)
splits (BiExpr op e1 e2) = (op, e1, e2)
splits (SinExpr op e) = (op, e, e)




anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f [] = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]

-- list all the patterns here
-- Con Int 
--                   | Var String 
--                   | Derivative Expression Expression 
--                   | SinExpr UnaryOp Expression 
--                   | BiExpr BinaryOp Expression Expression deriving Show




-- splits :: [a] -> [([a], [a])]
-- splits [] = [([],[])]
-- splits (a:as) = [([],a:as)] ++ [(a:as1,as2) | (as1,as2) <- splits as]

