module Main where

import Expressions
import Printer

main :: IO ()
-- main = someFunc

main = do putStrLn "Please input the problem as the form, (x, 2*x)." 
          str <- getLine
          showResult str

        --   case reads answer of
        --     ((n,_):_) -> putStrLn $ duckSong n
        --     _ -> putStrLn "Did not understand your input as a number. The function duckSong was not called."
