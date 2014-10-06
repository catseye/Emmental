Test Suite for Emmental
=======================

This test suite is written in the format of Falderal 0.9.  It is far from
exhaustive, but provides a basic sanity check on the language.

Emmental Tests
--------------

    -> Tests for functionality "Interpret Emmental Program"

    -> Functionality "Interpret Emmental Program" is implemented by
    -> shell command
    -> "bin/emmental %(test-body-file)"

### testProg 10 ###

Report whether the input is the character `M` or not.

    | #59#35#55#56#46#!;##1!;##2!;##3!;##4!;##5!;##6!;##7!#59#35#56#57#46#8!,#77-~?
    + M
    = Y

    | #59#35#55#56#46#!;##1!;##2!;##3!;##4!;##5!;##6!;##7!#59#35#56#57#46#8!,#77-~?
    + z
    = N
