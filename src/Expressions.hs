module Expressions
    ( someFunc
    ) where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)

type Parser = ParsecT Void String Identity

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- data Derivative = Derivative Expression Expression deriving Show
data UnaryOp = Sin | Cos | Tan | Ln | Neg deriving Show
data BinaryOp = Add | Sub | Mul | Div | Pow | Log deriving Show
data Expression = Con Int 
                  | Var String 
                  | Derivative Expression Expression 
                  | SinExpr UnaryOp Expression 
                  | BiExpr BinaryOp Expression Expression deriving Show



precedence1 = ["^"];
precedence2 = ["*", "/"];
precedence3 = ["+", "-"];

--  "x, x*3 + x^2"



ch :: Parser Char
ch = do {c <- satisfy (isAlpha); return c}

parserString :: Parser String
parserString = do{ space ;s<- some (ch); return s}

parserVar :: Parser Expression
parserVar = do{ space ;str <- parserString; return (Var str)}

digit :: Parser Int
digit = cvt <$> satisfy isDigit  --(elem ['0'])
  where cvt d = fromEnum d - (fromEnum '0')

digits :: Parser Int
digits = do ds <- some digit
            return (foldl1 shiftl ds)
         where shiftl m n = 10*m+n

parserCon :: Parser Expression
parserCon = do 
                space
                num <- digits
                return (Con num)


parserUnaryOp :: Parser UnaryOp
parserUnaryOp = space *> ((string "sin" *> return Sin)
     <|> (string "cos" *> return Cos)
     <|> (string "tan" *> return Tan)
     <|> (string "ln" *> return Ln)
     <|> (string "-" *> return Neg))

parserBinaryOp :: Parser BinaryOp
parserBinaryOp = space *> ((string "+" *> return Add)
     <|> (string "-" *> return Sub)
     <|> (string "*" *> return Mul)
     <|> (string "/" *> return Div)
     <|> (string "^" *> return Pow)
     <|> (string "log" *> return Log))


parserDerivative :: Parser Expression
parserDerivative = do 
                  space
                  vari <- parserExpression
                  string ","
                  space
                  expression <- parserExpression
                  return (Derivative vari expression)


parserSingleExpr :: Parser Expression
parserSingleExpr = do 
                  space
                  operator <- parserUnaryOp
                  expression <- parserExpression
                  return (SinExpr operator expression)

parserBiExpr :: Parser Expression
parserBiExpr = do 
               space
               expression1 <- parserCon
               operator <- parserBinaryOp
               expression2 <- parserCon
               return (BiExpr operator expression1 expression2)



parserExpression :: Parser Expression
-- parserExpression = space *> ( try( do{
--                                  exp1 <- char '(' *> parserExpression <* space  <* char ')';
--                                  op <- space *> parserBinaryOp;
--                                  exp2 <- space *> parserExpression;
--                                  return (BiExpr op exp1 exp2)})
--                               <|> try (do{
--                                      exp1 <- char '(' *> parserExpression <* space <* char ')';
--                                      return exp1;})
--                               <|> parserExpressionHelper)


parserExpression = space *> ( try( do{
                                 exp1 <- char '(' *> parserExpression <* space  <* char ')';
                                 op <- space *> parserBinaryOp;
                                 exp2 <- space *> parserExpression;
                                 return (BiExpr op exp1 exp2)})
                              <|> try (do{
                                     exp1 <- char '(' *> parserExpression <* space <* char ')';
                                     return exp1;})
                              <|> try( do{
                                          exp1 <-  parserExpression <* space ;
                                          op <- space *> parserBinaryOp;
                                          exp2 <- space *> parserExpression;
                                          return (BiExpr op exp1 exp2)})      
                              <|> parserExpressionHelper)


  --  parseTest parserExpression "(3 * 5) + 6" 
  --  parseTest parserExpression "(2 - (3 * 5))" 
parserExpressionHelper :: Parser Expression
parserExpressionHelper = space *> (try (parserBiExpr) 
                  <|> try (parserSingleExpr)
                  <|> try (parserCon)
                  <|> try (parserVar)
                  <|> try (parserDerivative)   
                  )