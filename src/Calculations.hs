module Calculations where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Rewrites
import Laws 

data Step = Step LawName Expression
data Calculation = Calc Expr [Step] 

calculate :: [Law] -> Expression -> Calculation
calculate laws e = Calc e (manyStep rws e)
    where rws e = [Step name e’ | Law name eqn <- sortedlaws, e’ <- rewrites eqn e, e' /= e]
          sortedlaws = sortLaws laws e

manyStep :: (Expression -> [Step]) -> Expression -> [Step] 
manyStep rws e
= case steps of 
    [] -> []
    (o@(Step _ e) : _) -> o:manyStep rws e 
  where steps = rws e