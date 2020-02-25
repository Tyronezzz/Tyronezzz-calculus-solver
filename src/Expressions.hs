{-# OPTIONS_GHC -Wall #-}

module Expressions where
--     ( someFunc
--     ) 


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
                  vari <- parserVar
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
               expression1 <- parserExpression
               operator <- parserBinaryOp
               expression2 <- parserExpression
               return (BiExpr operator expression1 expression2)



parserExpression :: Parser Expression
parserExpression = space *> (parserCon
                              <|> parserVar
                              <|> (char '(' *> parserExpressionHelper <* char ')'))
               
parserExpressionHelper :: Parser Expression
parserExpressionHelper = space *> (try (parserBiExpr) 
                  <|> try (parserDerivative)
                  <|> try (parserSingleExpr))





-- another version of parsing in book
-- brackets explicitly
-- space??
basicOps = addOp <|> mulOp


addOp :: ParsecT Void String Identity BinaryOp
addOp = space *> ((string "+" *> return Add)
     <|> (string "-" *> return Sub))

mulOp :: ParsecT Void String Identity BinaryOp
mulOp = space *> ((string "*" *> return Mul)
     <|> (string "/" *> return Div)
     <|> (string "log" *> return Log))

powOp :: ParsecT Void String Identity BinaryOp
powOp = space *> (string "^" *> return Pow)

expr :: Parser Expression
expr = space *> (term >>= rest)


rest :: Expression -> ParsecT Void String Identity Expression
rest e1 = space *> do {
                       p <- addOp;
                       e2 <- term;
                       rest (BiExpr p e1 e2)}
          <|> return e1

term :: ParsecT Void String Identity Expression
term = space *>  (factor2 >>= more)

more :: Expression -> ParsecT Void String Identity Expression
more e1 = space *> do {
                       p <- mulOp;
                       e2 <- factor2;
                       more (BiExpr p e1 e2)}
          <|> return e1

factor2 :: ParsecT Void String Identity Expression
factor2 = space *> (factor >>= powexpr)

powexpr :: Expression -> ParsecT Void String Identity Expression
powexpr e1 = space *> do {
                       p <- powOp;
                       e2 <- factor;
                       more (BiExpr p e1 e2)}
          <|> return e1

factor :: ParsecT Void String Identity Expression
factor = space *>  (try parserSinExpr <|> try oneDeriv <|> try ( string "(" *> expr <*space <* string ")"))


parserSinExpr :: Parser Expression
parserSinExpr = space *> try(do{  
                                   operator <- parserUnaryOp;
                                   expression <- expr;  -- parserVar??
                                   return (SinExpr operator expression)})
                    <|>try (do{
                              operator <- parserUnaryOp;
                              expression <- expr;
                              return (SinExpr operator expression)})
                    <|> try parserCon
                    <|> try parserVar

oneDeriv :: ParsecT Void String Identity Expression
oneDeriv =  do{ 
                _ <- string "(";
               vari <- space *> parserVar;
               _ <- string ",";
               expression <- space *> parserExpression;
               _ <- string ")";
               return (Derivative vari expression);}                        
