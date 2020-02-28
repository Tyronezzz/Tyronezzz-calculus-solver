# calculus-solver

## Data structures

```haskell

data UnaryOp = Sin | Cos | Tan | Ln | Neg deriving Show
data BinaryOp = Add | Sub | Mul | Div | Pow | Log deriving Show
data Expression = Con String | Var String | Derivative Expression Expression | SinExpr UnaryOp Expression | BiExpr BinaryOp Expression Expression deriving Show


data Law = Law String Equation
type Equation = (Expression, Expression)
data Step = Step LawName Expression
data Calculation = Calc Expression [Step] 
```

For calculation, we have binary operators like Add(+), Sub(-), Pow(^), etc. We also have unary operators like Sin, Cos, Ln, etc. For inputs with multiple operations like "2*x", we cannot ignore the *s. Otherwise, it will parse as "2x", a variable. 


## Example for Expression

```haskell
Input: "x * sin(x)"
After parsing: "BiExpr Mul (Var x) (SinExpr Sin (Var x))"

Input: "x + (6 / 5 - y) ^ 2"
After parsing: "BiExpr Add (Var x) (BiExpr Pow (BiExpr Sub (BiExpr Div (Con 6) (Con 5)) (Var y)) (Con 2))"

```


## Input format

We plan to use stdin for the inputs. The format of the input is as follows:

```haskell
The input would be a string. First, the variable which we would do the derivation on is given before a comma. Then the expression will be provided. For example:
"(x, x*y*sin(x))"
```


## Basic Parsing

We updated the parser using the method in book. This time, we can parse something like "3+4+5". 
The basic idea is we use 3 helper functions rest, more and powexpr to deal with addOp, mulOp and powOp. And it follows the order from powOp to addOp. This ensures the right order of calculation.  
Here is the example for parsing an expression. 

```haskell
Input: parseTest expr "(x, ((3 log x)+6))"
Output: Derivative (Var "x") (BiExpr Add (BiExpr Log (Con 3) (Var "x")) (Con 6))
```


## Reason with configurable rules

Now we have finished the rewrites function. But we are not sure about match and substitution. For match, should we return [Subst]? If so, we have a list of possible substitutions. For example, for the input "1+2+3", and there is an add rule x + y = ..., then it should return [[(x, 1+2), (y, 3)], [(x,1), (y,2+3)]]. But we are not sure how to get the [Subst].


Also did some work on pretty print. 







<!-- Since our data structure is different from what we have learned in the book and lectures, our Expression is not made of list, a lot of functions that have list operations cannot be used in our project. We have to come up with our own Rewrites, Matching and Substitutions modules. We are having some troubles implementing them. I am wondering if it better to change our data structure in order to make it doable? Should we stay or change our data structure? Thank you. -->



<!-- 
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