module Rewrites where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Laws 

rewrites :: Equation -> Expression -> [Expression]
rewrites = 



anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f [] = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]
            
splits :: [a] -> [([a], [a])]
splits [] = [([],[])]
splits (a:as) = [([],a:as)] ++ [(a:as1,as2) | (as1,as2) <- splits as]

segments as = [(as1,as2,as3)
              | (as1,bs) <- splits as,
                (as2,as3) <- splits bs]

rewritesSeg :: Equation -> 