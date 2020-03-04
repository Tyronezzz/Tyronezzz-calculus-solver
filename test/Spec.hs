import Test.Tasty
import Test.Tasty.HUnit
import Expressions
import Match

main :: IO ()
main = defaultMain (testGroup "Library Tests" [test1, test2])

test1, test2 :: TestTree


-- tests for match
-- expected actual
test1 = testCase "match a+b with law: x+y" (assertEqual "guess not"  ([[(Var "x", Var "a"), (Var "y", Var "b")]]) (Match.match (BiExpr Add (Var "x") (Var "y")) (BiExpr Add (Var "a") (Var "b"))) )
test2 = testCase "match sin (y^2) with law: sin x" (assertEqual "guess not"  ([[(Var "x", (BiExpr Pow (Var "y") (Con 2)))]]) (Match.match (SinExpr Sin (Var "x")) (SinExpr Sin (BiExpr Pow (Var "y") (Con 2))) ) )





