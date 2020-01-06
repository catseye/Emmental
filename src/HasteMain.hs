{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste.DOM (withElems, getValue, setProp)
import Haste.Events (onEvent, MouseEvent(Click))
import Haste.Foreign (ffi)

import Emmental (emmentalWithIO)


getCh :: IO Char
getCh = ffi "(function() {return 'A';})"

putCh :: Char -> IO ()
putCh = ffi "(function(c) {console.log(c);})"

main = withElems ["prog", "result", "run-button"] driver

driver [progElem, resultElem, runButtonElem] =
    onEvent runButtonElem Click $ \_ -> do
        Just prog <- getValue progElem
        r <- emmentalWithIO (getCh) (putCh) prog
        setProp resultElem "textContent" $ show $ r
