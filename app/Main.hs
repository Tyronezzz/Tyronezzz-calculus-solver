module Main where

import Expressions
import Printer

main :: IO ()
main = do putStrLn "\n******************\nCalculus Solver\n******************\nPlease input the problem as the form, (x, 2*x).\n" 
          str <- getLine
          showResult str