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
<<<<<<< HEAD

-- list all the patterns here
-- Con Int 
--                   | Var String 
--                   | Derivative Expression Expression 
--                   | SinExpr UnaryOp Expression 
--                   | BiExpr BinaryOp Expression Expression deriving Show


splits (BiExpr op e1 e2) = [([], [(BiExpr op e1 e2)])]  ++ [ (e1:as1, as2) | (as1, as2) <- splits e2]

splits (SinExpr op e) = [ ([], [(SinExpr op e)]) ]  ++ [ (as1, as2) | (as1, as2) <- splits e] 

splits (Var s) = [ ([], [Var s] ), ([Var s], []) ]

splits (Con s) = [ ([], [Con s] ), ([Con s], []) ]

-- splits :: [a] -> [([a], [a])]
-- splits [] = [([],[])]
-- splits (a:as) = [([],a:as)] ++ [(a:as1,as2) | (as1,as2) <- splits as]





-- segments as = [(as1,as2,as3)
--               | (as1,bs) <- splits as,
--                 (as2,as3) <- splits bs]

-- rewritesSeg :: Equation -> 
=======
            
>>>>>>> 127df6fdf9e63b6e3dbb83de51a279dde73f0c71
