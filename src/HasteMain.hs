{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste.DOM (withElems, getValue, setProp)
import Haste.Events (onEvent, MouseEvent(Click))
import Haste.Foreign (ffi)

import Language.Emmental (emmentalWithIO)


getCh :: IO Char
getCh = ffi "(function() {var i=document.getElementById('prog-input'); var s=i.value; i.value=s.substring(1); return s.charAt(0);})"

putCh :: Char -> IO ()
putCh = ffi "(function(c) {var o=document.getElementById('prog-output'); var s=o.value; o.value=s+c;})"

main = withElems ["prog", "result", "run-button"] driver

driver [progElem, resultElem, runButtonElem] =
    onEvent runButtonElem Click $ \_ -> do
        Just prog <- getValue progElem
        r <- emmentalWithIO (getCh) (putCh) prog
        setProp resultElem "textContent" $ show $ r
