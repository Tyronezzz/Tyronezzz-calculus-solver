import Test.Tasty
import Test.Tasty.HUnit
import Expressions
import Match
import Substitutions

main :: IO ()
main = defaultMain (testGroup "Library Tests" [test1, test2, test3
                                               ,test4, test5, test6
                                               ,test7 ])

test1, test2, test3, test4, test5, test6, test7 :: TestTree


-- tests for match
-- expected actual
test1 = testCase "match a+b with law: x+y" (assertEqual "guess not"  ([[(Var "x", Var "a"), (Var "y", Var "b")]]) (Match.match (BiExpr Add (Var "x") (Var "y")) (BiExpr Add (Var "a") (Var "b"))) )
test2 = testCase "match sin (y^2) with law: sin x" (assertEqual "guess not"  ([[(Var "x", (BiExpr Pow (Var "y") (Con 2)))]]) (Match.match (SinExpr Sin (Var "x")) (SinExpr Sin (BiExpr Pow (Var "y") (Con 2))) ) )
test3 = testCase "not match a+b with law: x/y" (assertEqual "guess not"  [] (Match.match (BiExpr Div (Var "x") (Var "y")) (BiExpr Add (Var "a") (Var "b"))) )

-- tests for compatible
test4 = testCase "compatible [] [(Var x, Var a)]" (assertEqual "guess not"  True (compatible [] [(Var "x", Var "a")]) )
test5 = testCase "compatible [(Var x, Var b)] [(Var x, Var a)]" (assertEqual "guess not"  False (compatible [(Var "x", Var "b")] [(Var "x", Var "a")]) )
test6 = testCase "compatible [(Var x, Var b)] [(Var y, Var a)]" (assertEqual "guess not"  True (compatible [(Var "x", Var "b")] [(Var "y", Var "a")]) )

-- apply
sub = [(Var "x",Con 2),(Var "y",Con 3)]
e = BiExpr Add (Var "y") (Var "x")
test7 = testCase "apply (y+x) [(Var x,Con 2),(Var y,Con 3)]" (assertEqual "guess not"  (BiExpr Add (Con 3) (Con 2)) (apply e sub) )