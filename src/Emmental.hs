--
-- Emmental.hs
-- Interpreter for the Emmental Programming Language
-- Chris Pressey, Cat's Eye Technologies
-- This work is in the public domain.  See UNLICENSE for more information.
--

module Emmental where

import Prelude (
    Show, Char, IO, putStr, getChar, putChar, last, init,
    (++), (+), (-), (*), mod, div, return, take, reverse, show
  )

import qualified Data.Map as Map
import qualified Data.Char as Char


-----------------------------------------------------------------------
-- ============================ Symbols ============================ --
-----------------------------------------------------------------------

type Symbol = Char


-----------------------------------------------------------------------
-- ======================== Program States ========================= --
-----------------------------------------------------------------------

data State = State {
    stack :: [Symbol],
    queue :: [Symbol],
    getCh :: IO Char,
    putCh :: Char -> IO ()
}

instance Show State where
    show State{ stack=stack, queue=queue } =
        "State {stack = " ++ (show stack) ++ ", queue = " ++ (show queue) ++ "}"

pop st@State{ stack=(head:tail) } = (head, st{ stack=tail })
push st@State{ stack=stack } sym  = st{ stack=(sym:stack) }

popString st@State{ stack=(';':tail) } = ([], st{ stack=tail })
popString st@State{ stack=(head:tail) } =
    let
        (string, state') = popString st{ stack=tail }
    in
        (string ++ [head], state')

enqueue st@State{ queue=queue } symbol = st{ queue=(symbol:queue) }
dequeue st@State{ queue=queue } =
    let
        symbol = last queue
        queue' = init queue
    in
        (symbol, st{ queue=queue' })


-----------------------------------------------------------------------
-- ========================= Interpreters ========================== --
-----------------------------------------------------------------------

data Interpreter = Interp (Map.Map Symbol Operation)

fetch (Interp map) sym = Map.findWithDefault (fitRegOp opNop) sym map
supplant (Interp map) sym op = (Interp (Map.insert sym op map))


-----------------------------------------------------------------------
-- ========================== Operations =========================== --
-----------------------------------------------------------------------

type Operation = State -> Interpreter -> IO (State, Interpreter)

composeOps :: Operation -> Operation -> Operation

composeOps op1 op2 = f where
    f state interpreter = do
        (state', interpreter') <- op1 state interpreter
        op2 state' interpreter'

createOp :: Interpreter -> [Symbol] -> Operation

createOp interpreter [] = 
    (fitRegOp opNop)
createOp interpreter (head:tail) =
    composeOps (fetch interpreter head) (createOp interpreter tail)

--
-- It's useful for us to express a lot of our operators as non-monadic
-- functions that don't affect the interpreter.  This is a little "adapter"
-- function that lets us create monadic functions with the right signature
-- from them.
--

fitRegOp :: (State -> State) -> Operation

fitRegOp regop = f where
    f state interpreter =
        let
            state' = regop state
        in
            do return (state', interpreter)


------------------------------------------------------------
--------------- The operations themselves. -----------------
------------------------------------------------------------

--
-- Redefine the meaning of the symbol on the stack with
-- a mini-program also popped off the stack.
--

opSupplant state interpreter =
    let
        (opSym, state') = pop state
        (newOpDefn, state'') = popString state'
        newOp = createOp interpreter newOpDefn
    in
        do return (state'', supplant interpreter opSym newOp)

--
-- Execute the symbol on the stack with the current interpreter.
--

opEval state interpreter =
    let
        (opSym, state') = pop state
        newOp = createOp interpreter [opSym]
    in
        newOp state' interpreter

--
-- I/O.
--

opInput state@State{ getCh=getCh } interpreter = do
    symbol <- getCh
    do return (push state symbol, interpreter)

opOutput state@State{ putCh=putCh } interpreter =
    let
        (symbol, state') = pop state
    in do
        putCh symbol
        return (state', interpreter)

--
-- Primitive arithmetic.
--

opAdd state =
    let
        (symA, state') = pop state
        (symB, state'') = pop state'
    in
        push state'' (Char.chr (((Char.ord symB) + (Char.ord symA)) `mod` 256))

opSubtract state =
    let
        (symA, state') = pop state
        (symB, state'') = pop state'
    in
        push state'' (Char.chr (((Char.ord symB) - (Char.ord symA)) `mod` 256))

discreteLog 0 = 8
discreteLog 1 = 0
discreteLog 2 = 1
discreteLog n = (discreteLog (n `div` 2)) + 1

opDiscreteLog state =
    let
        (symbol, state') = pop state
    in
        push state' (Char.chr (discreteLog (Char.ord symbol)))

--
-- Stack manipulation.
--

--
-- Pop the top symbol of the stack, make a copy of it, push it back onto the
-- stack, and enqueue the copy onto the queue.
--

opEnqueueCopy state =
    let
        (sym, _) = pop state
    in
        enqueue state sym

--
-- Dequeue a symbol from the queue and push it onto the stack.
--

opDequeue state =
    let
        (sym, state') = dequeue state
    in
        push state' sym

--
-- Duplicate the top symbol of the stack.
--

opDuplicate state =
    let
        (symbol, _) = pop state
    in
        push state symbol

--
-- Miscellaneous operations.
--

opNop state =
    state

--
-- Parameterizable operations.
--

opPushValue value state =
    push state (Char.chr value)

opAccumValue value state =
    let
        (sym, state') = pop state
        value' = ((Char.ord sym) * 10) + value
    in
        push state' (Char.chr (value' `mod` 256))


-----------------------------------------------------------------------
-- ===================== Debugging Functions ======================= --
-----------------------------------------------------------------------

type Debugger = State -> Interpreter -> IO ()

debugNop s i = do
    return ()

debugPrintState s i = do
    putStr ((show s) ++ "\n")
    return ()


-----------------------------------------------------------------------
-- ============================ Executor =========================== --
-----------------------------------------------------------------------

execute :: [Symbol] -> State -> Interpreter -> Debugger -> IO (State, Interpreter)

execute [] state interpreter debugger =
    return (state, interpreter)
execute (opSym:program') state interpreter debugger =
    let
        operation = fetch interpreter opSym
    in do
        (state', interpreter') <- operation state interpreter
        debugger state' interpreter'
        execute program' state' interpreter' debugger


-----------------------------------------------------------------------
-- ====================== Top-Level Function ======================= --
-----------------------------------------------------------------------

initialInterpreter = Interp
  (Map.fromList
    [
        ('.', opOutput),
        (',', opInput),

        ('#', fitRegOp (opPushValue 0)),
        ('0', fitRegOp (opAccumValue 0)),
        ('1', fitRegOp (opAccumValue 1)),
        ('2', fitRegOp (opAccumValue 2)),
        ('3', fitRegOp (opAccumValue 3)),
        ('4', fitRegOp (opAccumValue 4)),
        ('5', fitRegOp (opAccumValue 5)),
        ('6', fitRegOp (opAccumValue 6)),
        ('7', fitRegOp (opAccumValue 7)),
        ('8', fitRegOp (opAccumValue 8)),
        ('9', fitRegOp (opAccumValue 9)),

        ('+', fitRegOp opAdd),
        ('-', fitRegOp opSubtract),
        ('~', fitRegOp opDiscreteLog),

        ('^', fitRegOp opEnqueueCopy),
        ('v', fitRegOp opDequeue),
        (':', fitRegOp opDuplicate),

        ('!', opSupplant),
        ('?', opEval),

        (';', fitRegOp (opPushValue (Char.ord ';')))
    ]
  )

initialState = State { stack=[], queue=[], getCh=getChar, putCh=putChar }

emmental string = do
    (state, interpreter) <- execute string initialState initialInterpreter debugNop
    return state

emmentalWithIO getCh putCh string =
    let
        i = initialState
        i' = i{ getCh=getCh, putCh=putCh }
    in do
        (state, interpreter) <- execute string i' initialInterpreter debugNop
        return state

debug string = do
    (state, interpreter) <- execute string initialState initialInterpreter debugPrintState
    return state


-----------------------------------------------------------------------
-- ========================== Test Cases =========================== --
-----------------------------------------------------------------------

--
-- Drivers for test cases.  'demo' runs them straight, whereas 'test'
-- uses the debugger.
--

demo n = emmental (testProg n)

test n = debug (testProg n)

--
-- Here we introduce a bit of a cheat, in order to make writing
-- complex Emmental programs tolerable.  You can still see the
-- programs in their full glory by executing "show (testProg n)".
--

quote [] = []
quote (symbol:rest) = "#" ++ (show (Char.ord symbol)) ++ (quote rest)

--
-- Add one and one.
--

testProg 1 = "#1#1+"

--
-- Redefine & as "+".
--

testProg 2 = ";#43#38!#1#1&"            -- 59,43,38 ==> ";+&"

--
-- Redefine 0 as "9".
--

testProg 3 = ";#57#48!#0"               -- 59,57,48 ==> ";90"

--
-- Redefine 0 as "#48?".  This results in an infinite loop when 0 is executed.
--

testProg 4 = ";#35#52#56#63#48!0"       -- 59,35,52,56,63,48 ==> ";#48?0"

--
-- Redefine $ as ".#36?".  This results in a loop that pops symbols and
-- and prints them, until the stack underflows, when $ is executed.
--

testProg 5 = ";#46#35#51#54#63#36! #65#66#67#68#69$"

--
-- Duplicate the top stack element (assuming an empty queue.)
-- This shows that the : operation is not strictly necessary
-- (when you know the size of the queue.)
--

testProg 6 = "#65^v"

--
-- Discard the top stack element (assuming more than one element
-- on the stack, and an empty queue.)
--

testProg 7 = "#33#123^v-+"

--
-- Swap the top two elements of the stack (assuming an empty queue.)
--

testProg 8 = "#67#66#65^v^-+^^v^v^v-+^v-+^v-+vv"

--
-- Input a symbol.  Report whether its ASCII value is even or odd.
--

testProg 9 = (quote ";^v:") ++ "!" ++                           -- : = dup
             (quote ";#69.") ++ "#!" ++                         -- NUL = print "E"
             (quote ";#79.") ++ "#128!" ++                      -- \128 = print "O"
             (quote (";" ++ (take 127 [':',':'..]) ++           -- m = mul by 128
             (take 127 ['+','+'..]) ++ "m")) ++ "!" ++
             ",m?"

--
-- Input a symbol.  Report whether it is M or not.
--

testProg 10 = (quote ";#78.") ++ "#!" ++                        -- NUL = print "N"
              ";##1!" ++                                        -- SOH = same as NUL
              ";##2!" ++                                        -- STX = same as NUL
              ";##3!" ++                                        -- ETX = same as NUL
              ";##4!" ++                                        -- EOT = same as NUL
              ";##5!" ++                                        -- ENQ = same as NUL
              ";##6!" ++                                        -- ACK = same as NUL
              ";##7!" ++                                        -- BEL = same as NUL
              (quote ";#89.") ++ "#8!" ++                       -- BS = print "Y"
              ",#77-~?"

--
-- Same as testProg 5, except stop printing when a NUL is
-- encountered, instead of just underflowing the stack.
--

testProg 11 = ";" ++ (quote ":~?$") ++ "!" ++                    -- $ = dup & test
              ";" ++ (quote ".$") ++ "#!" ++                     -- NUL = print & repeat
              ";#0#1!" ++                                        -- SOH = same as NUL
              ";#0#2!" ++                                        -- STX = same as NUL
              ";#0#3!" ++                                        -- ETX = same as NUL
              ";#0#4!" ++                                        -- EOT = same as NUL
              ";#0#5!" ++                                        -- ENQ = same as NUL
              ";#0#6!" ++                                        -- ACK = same as NUL
              ";#0#7!" ++                                        -- BEL = same as NUL
                                                                 -- BS = stop (nop)
              "#0" ++ (quote (reverse "Hello!")) ++ "$"
