Test Suite for Emmental
=======================

This test suite is written in the format of Falderal 0.9.  It is far from
exhaustive, but provides a basic sanity check on the language.

Emmental Tests
--------------

    -> Functionality "Interpret Emmental Program" is implemented by
    -> shell command
    -> "bin/emmental %(test-body-file)"

    -> Functionality "Interpret Emmental Program and Show Final State"
    -> is implemented by shell command
    -> "bin/emmental -r %(test-body-file)"

    -> Tests for functionality "Interpret Emmental Program and Show Final State"

Add one and one.

    | #1#1+
    = State "\STX" ""

Redefine `&` as `+`. (`59,43,38 ==> ";+&"`)

    | ;#43#38!#1#1&
    = State "\STX" ""

Redefine `0` as `9`. (`59,57,48 ==> ";90"`)

    | ;#57#48!#0            
    = State "\t" ""

Redefine `$` as `.#36?`.  This results in a loop that pops symbols and
and prints them, until the stack underflows, when `$` is executed.

    | ;#46#35#51#54#63#36! #65#66#67#68#69$
    ? pop

Duplicate the top stack element (assuming an empty queue.)
This shows that the `:` operation is not strictly necessary
(when you know the size of the queue.)

    | #65^v
    = State "AA" ""

Discard the top stack element (assuming more than one element
on the stack, and an empty queue.)

    | #33#123^v-+
    = State "!" ""

Swap the top two elements of the stack (assuming an empty queue.)

    | #67#66#65^v^-+^^v^v^v-+^v-+^v-+vv
    = State "BAC" ""

    -> Tests for functionality "Interpret Emmental Program"

Input a symbol.  Report whether its ASCII value is even or odd.

    | #59#94#118#58!#59#35#54#57#46#!#59#35#55#57#46#128!#59#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#109!,m?
    + @
    = E

    | #59#94#118#58!#59#35#54#57#46#!#59#35#55#57#46#128!#59#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58#58
    | #58#58#58#58#58#58#58#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43
    | #43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#43#109!,m?
    + A
    = O

Input a symbol.  Report whether it is M or not.

    | #59#35#55#56#46#!;##1!;##2!;##3!;##4!;##5!;##6!;##7!#59#35#56#57#46#8!
    | ,#77-~?
    + M
    = Y

    | #59#35#55#56#46#!;##1!;##2!;##3!;##4!;##5!;##6!;##7!#59#35#56#57#46#8!
    | ,#77-~?
    + z
    = N

Redefine `$` such that this results in a loop that pops symbols and
and prints them, until the stack underflows, when `$` is executed,
except stop printing when a NUL is encountered, instead of just
underflowing the stack.  (See `testProg 5` and `testProg 11` in `Emmental.hs`
for a more lucid explanation.)

    | ;#58#126#63#36!;#46#36#!;#0#1!;#0#2!;#0#3!;#0#4!;#0#5!;#0#6!;#0#7!
    | #0#33#111#108#108#101#72$
    = Hello!
