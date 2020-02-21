module Laws
    ( parserLaw
    ) where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)
import Expressions
import Data.List (partition)

data Law = Law String Equation deriving Show
type Equation = (Expression, Expression)

parserLaw :: Parser Law
parserLaw = do{
                lawName <- parserString;
                _ <- space *> char ':';
                eq <- space *> parserEquation;
                return (Law lawName eq)}

parserEquation = do{
                    exp1 <- space *> expr;
                    _ <- space *> char '=';
                    exp2 <- space *> expr;
                    return (exp1, exp2)}


-- sortLaws laws = simple ++ others ++ defns
--      where
--      (simple, nonsimple) = partition isSimple laws
--      (defns, others)     = partition isDefn nonsimple


partition p xs = (filter p xs, filter (not . p) xs)


-- isSimple (Law _ (Expression as1,Expression as2)) = length as1 > length as2

-- isDefn (Law _ (Compose [Con f es], _)) = all isVar es
-- isDefn _ = False
-- isVar (Compose [Var _]) = True
-- isVar _                 = False