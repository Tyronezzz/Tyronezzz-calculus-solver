# calculus-solver


## Data structures

```haskell

data UnaryOp = Sin | Cos | Tan | Ln | Neg deriving Show
data BinaryOp = Add | Sub | Mul | Div | Pow | Log deriving Show
data Expression = Con String | Var String | Derivative Expression Expression | SinExpr UnaryOp Expression | BiExpr BinaryOp Expression Expression deriving Show



data Law = Law String Equation
type Equation = (Expression, Expression)
data Step = Step LawName Expression
data Calculation = Calc Expr [Step] 
```

For calculation, we have binary operators like Add(+), Sub(-), Pow(^), etc. We also have unary operators like Sin, Cos, Ln, etc. For inputs with multiple operations like "2*x", we cannot ignore the *s. Otherwise, it will parse as "2x", a variable. 


## Example for Expression

```haskell
Input: "x * sin(x)"
After parsing: "BiExpr Mul (Var x) (SinExpr Sin (Var x))"

Input: "x + (6 / 5 - y) ^ 2"
After parsing: "BiExpr Add (Var x) (BiExpr Pow (BiExpr Sub (BiExpr Div (Con 6) (Con 5)) (Var y)) (Con 2))"

```

<!-- BiExpr Add (BiExpr Add (Con 1) (Con 2)) (Con 3) -->


## Input format

We plan to use stdin for the inputs. The format of the input is as follows:

```haskell
The input would be a string. First, the variable which we would do the derivation on is given before a comma. Then the expression will be provided. For example:
"(x, x * y * sin(x))"
```


## Plans

After designing the data structures, we will work on laws, calculations and rewrite. 



## Basic Parsing

We changed our `Expression` in order to solve the problem that in your feedback to the following:

```haskell
data Expression = Con Int 
                  | Var String 
                  | Derivative Expression Expression 
                  | SinExpr UnaryOp Expression 
                  | BiExpr BinaryOp Expression Expression deriving Show
```

So, now each step could also include a derivative expression. 

Also, we wrote a parser that could parse our problems. For example:

```haskell
Input: parseTest parserExpression "(x, ((3 log x) + 6))"
Output: Derivative (Var "x") (BiExpr Add (BiExpr Log (Con 3) (Var "x")) (Con 6))
```

