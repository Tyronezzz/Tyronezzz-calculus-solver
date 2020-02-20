module Lib
    ( someFunc, deriv
    ) where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)

type Parser = ParsecT Void String Identity

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Derivative = Derivative Expression Expression
data UnaryOp = Sin | Cos | Tan | Ln 
data BinaryOp = Add | Sub | Mul | Div | Pow | Log
data Expression = Con String | Var String | SinExpr UnaryOp Expression | BiExpr BinaryOp Expression Expression


--  "x, x*3 + x^2"

deriv = free_variable <* string "," >>= expr

ch :: Parser Char
ch = do {c <- satisfy (isAlpha); return c}

unary_op = do{ s<- some (ch); return s}

free_variable = do{ s<- some (ch); return s}

-- digit = cvt <$> (satisfy isDigit) 
--   where cvt d = fromEnum d - (fromEnum '0')

-- digit = do {c <- satisfy (isDigit); return c}
digits = do{s<- some (digitChar); return s}

expr = do{
          num <- digits;
          op <- get_op;
          }
       <|> (unary_op >>=)

--  num/ freevariable /sin,ln