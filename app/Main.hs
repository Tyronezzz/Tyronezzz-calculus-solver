-- {-# LANGUAGE OverloadedStrings #-}
module Main where

import Expressions
import Printer
import Text.Pandoc.Builder
import Text.Pandoc
import Data.Monoid ((<>), mempty, mconcat)
import Data.Aeson
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.List (intersperse)
import Printer
import Calculations




main :: IO ()
main = do putStrLn "\n******************\nCalculus Solver\n******************\nPlease input the problem as the form, (x, 2*x).\n" 
          str <- getLine
          showResult str



-- mydoc = doc $ header 1 (text "Hello!")
--            <> para (emph (text "hello world") <> text ".")
           

-- main = ggFunc
--   print mydoc

