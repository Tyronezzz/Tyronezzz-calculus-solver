{-# LANGUAGE OverloadedStrings #-}
module Main where

import Expressions
import Printer
-- import Text.Pandoc.Builder
-- import Text.Pandoc


main :: IO ()
main = do putStrLn "\n******************\nCalculus Solver\n******************\nPlease input the problem as the form, (x, 2*x).\n" 
          str <- getLine
          showResult str


-- main =  do
--     letter <- getLetter
--     temp <- readFile "template.tex"
--     let str_should_have_something = writeLaTeX (def {writerStandalone = True, writerTemplate = temp}) letter
--     print str_should_have_something
--     mybytes <- export temp letter

--     case mybytes of Right b -> BL.writeFile "mypdf.pdf" b
--                     Left  _ -> putStrLn "Export error"