module Rewrites where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Laws 

rewrites :: Equation -> Expression -> [Expression]
rewrites eqn (Con n) = []
rewrites eqn (Var v) = []
rewrites eqn (Derivative v e) = map Derivative v (rewrites eqn e)
rewrites eqn (BiExpr biOp e1 e2) = 
rewrites eqn (SinExpr uOp e) = 



anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f [] = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]
            
