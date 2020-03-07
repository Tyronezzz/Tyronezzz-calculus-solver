# calculus-solver

This project is used for solving derivitive problems. Now, it supports +, -, *, /, ^, sin, cos, ln operators.


## Design the structure
Based on our data type, we build the program with some modules. 
- Expressions: parse the input string and output the Expression type.
- Laws: Give the derivitive laws.
- Match: Match the current expression with the left hand side equation. If they are match, return a substitution list. 
- Printer: Print the data types.
- Substitutions: Substitude the right hand side equation with the pairs in Subst.
- Rewrites: Match the expression with the equation then get the new expression according to the law.
- Calculations: Do the calculations and get many steps of the derivition. 


### Data structures

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



### Basic Parsing

We updated the parser using the method in book. This time, we can parse something like "3+4+5". 
The basic idea is we use 3 helper functions rest, more and powexpr to deal with addOp, mulOp and powOp. And it follows the order from powOp to addOp. This ensures the right order of calculation.  
Here is the example for parsing an expression. 

```haskell
Input: parseTest expr "(x, ((3 log x)+6))"
Output: Derivative (Var "x") (BiExpr Add (BiExpr Log (Con 3) (Var "x")) (Con 6))
```

### Laws
We are using the following laws. The first 9 laws are used to do the derivitive operation and the rest is used to simply the result.

- "add : (x, a+b)=(x, a)+(x, b)",
- "chain rule : (x, a*b)=(x, a)*b+a*(x, b)",
- "sin : (x, sin(a))=cos(a)*(x, a)",
- "cos : (x, cos(a))=-sin(a)*(x, a)", 
- "ln : (x, ln(a))=(1/a)*(x, a)",
- "power : (x, a^b)=a^b * (x, b*ln(a))", 
- "derivative_self : (x, x)=1",
- "derivative_not_self : (x, y)=0",
- "constant : (x, a)=0",

- "zero mul : 0*x = 0",
- "zero add : 0+x = x",
- "one mul : 1*x = x"






## Bugs fixed
- dz x^y
- deriv const law
- parse input for para in calculate
- compatible for multi-variables
- pretty print
- spaces



<!-- Now we have finished the rewrites function. But we are not sure about match and substitution. For match, should we return [Subst]? If so, we have a list of possible substitutions. For example, for the input "1+2+3", and there is an add rule x + y = ..., then it should return [[(x, 1+2), (y, 3)], [(x,1), (y,2+3)]]. But we are not sure how to get the [Subst].
Also did some work on pretty print.  -->

### Special feature

In this project, besides the terminal output, we also used `pandoc` to generate a `result.docx` showing the calculation. For the pdf version, there are some bugs that we haven't fixed yet. Here is the result in `result.docx` file. 


![result](https://user-images.githubusercontent.com/15730783/76132998-e4428100-5fe3-11ea-916f-9d73a3ed51f7.png)


<!-- Since our data structure is different from what we have learned in the book and lectures, our Expression is not made of list, a lot of functions that have list operations cannot be used in our project. We have to come up with our own Rewrites, Matching and Substitutions modules. We are having some troubles implementing them. I am wondering if it better to change our data structure in order to make it doable? Should we stay or change our data structure? Thank you. -->







## Run the program

To run the program using

```
stack run
```

Then input your question, for example, (x, x^2), for the derivative of x^2 to x. Press the enter and then you can get the steps.Be careful, you cannot use key left, key right. You can only solve one problem in this program.
*For unary expressions like sin(x), you need to type as (sin (x)) with the outer brackets.*


## Test the program

To test the program using

```
stack test
```



## Input format

We plan to use stdin for the inputs. The format of the input is as follows:

```haskell
The input would be a string. First, the variable which we would do the derivation on is given before a comma. Then the expression will be provided. For example:
"(x, x*y*sin(x))"
```

Here is the calculation result showing in terminal. 

![result1](https://user-images.githubusercontent.com/15730783/76132985-cd9c2a00-5fe3-11ea-8128-fe58b8d1f7d7.png)


 

