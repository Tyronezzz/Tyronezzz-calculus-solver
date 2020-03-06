module Calculations where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Rewrites
import Laws 

--data structures for Calculation
data Step = Step LawName Expression deriving Show
data Calculation = Calc Expression [Step]  deriving Show
type LawName = String


-- do the calculation and get the steps
calculate :: [Law] -> Expression -> Calculation
calculate laws e = Calc e (manyStep rws e)
    where rws e = [Step name e_new | Law name eqn <- sortedlaws, e_new <- rewrites eqn e, e_new /= e]
          sortedlaws = sortLaws laws e

-- get the steps. It works as dfs searching
manyStep :: (Expression -> [Step]) -> Expression -> [Step] 
manyStep rws e = case steps of 
                   [] -> []
                   (o@(Step _ e) : _) -> o:manyStep rws e 
                   where steps = rws e