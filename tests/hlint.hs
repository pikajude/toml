module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = (`unless` exitFailure) . null =<< hlint
    [ "src", "tests"
    , "--cpp-include=include"
    ]
