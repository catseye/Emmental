module Main where

import System.Environment
import Language.Emmental

main = do
    args <- getArgs
    case args of
        [fileName] -> do
            c <- readFile fileName
            emmental c
            return ()
        ["-d", fileName] -> do
            c <- readFile fileName
            debug c
            return ()
        ["-r", fileName] -> do
            c <- readFile fileName
            r <- emmental c
            putStrLn (show r)
        _ -> do
            putStrLn "Usage: emmental [-d|-r] <filename.emmental>"
