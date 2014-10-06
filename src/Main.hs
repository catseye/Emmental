module Main where

import System.Environment
import Emmental

main = do
    [fileName] <- getArgs
    c <- readFile fileName
    emmental c
