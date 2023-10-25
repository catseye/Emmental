The Emmental Programming Language
=================================

_Try it online_ [@ catseye.tc](https://catseye.tc/installation/Emmental)
| _Wiki entry_ [@ esolangs.org](https://esolangs.org/wiki/Emmental)
| _See also:_ [Mascarpone](https://codeberg.org/catseye/Mascarpone#the-mascarpone-programming-language)

- - - -

Introduction
------------

Emmental is a self-modifying programming language. That is not to say
that it is a language in which programs are self-modifying; rather, it is
the language itself, as defined by a meta-circular interpreter, that can
be modified during the course of a running program. Indeed, this is how
Emmental, without conventional conditional and repetition/recursion
constructs, achieves Turing-completeness.

Meta-circular Interpreters
--------------------------

One way to attempt to define a language is by giving what's called a
*meta-circular interpreter* (often shortened to "MCI" in this document.)
This is an interpreter for some language which is written in that same
language (or at least, a language which is very close to it.)

Meta-circular interpreters were a popular way to the describe the
semantics of programming languages, especially LISP-like languages, and
especially before the advent of denotational semantics. The term
"meta-circular" was apparently coined by John C. Reynolds in his paper
"Definitional Interpreters for Higher-Order Programming Languages" (1972
Proceedings ACM National Conference.)

Of course, in the real world, MCI's are not often used. They certainly
*can* be used: if you have a working Scheme interpreter that came with
your computer system, there is nothing stopping you from writing another
Scheme interpreter in Scheme, and running your programs on your
interpreter (which is itself running on your system's interpreter.)
However, this is quite a bit less efficient due to the duplication of
effort. A somewhat more realistic case might be if your system came
with, say, a Scheme compiler. You might then feed your Scheme
interpreter (written in Scheme) through that to make a native Scheme
interpreter, and use that to interpret your programs. (In this setup,
the interpreter is usually described as "self-hosting" rather than
"meta-circular".)

But, as should be obvious, you already need an implementation of Scheme
for your Scheme interpreter written in Scheme to be of much practical
use to you. If your meta-circular interpreter is all you have, you won't
be able to use it to run or understand Scheme programs. Because the MCI
is defined in terms of itself, you'll need some other source of
"understanding how it works" to make it complete. This understanding
might come from an implementation in some other programming language, or
a specification in some formal language, or a description in some
natural language, or simply from intuition — but it has to come from
somewhere.

Assuming that we do have that external source of understanding, the
meta-circular interpreter can come in quite handy in codifying the
semantics of the language. And, in Emmental's case, *altering* those
semantics: Emmental's MCI supports operations which instruct Emmental's
MCI to modify its behaviour.

Interpreter Structure
---------------------

To describe the structure of Emmental's MCI, we first examine the
general structure of interpreters. If you've ever written a virtual
machine executor in, say, C, you've noticed that it has the general form

        pc = start;
        while (!done) {
            switch (instruction[pc]) {
                case INSTR_ONE:
                    /* implement semantics of INSTR_ONE */
                    pc += advance(INSTR_ONE);
                    break;
                case INSTR_TWO:
                    /* implement semantics of INSTR_TWO */
                    pc += advance(INSTR_TWO);
                    break;
                /* ... */
                case INSTR_HALT:
                    done = 1;
                    break;
                default:
                    perror("Invalid opcode");
            }
        }

Note that `advance()` is some function that computes how far the program
counter is advanced on that instruction. This value is typically +1 for
most instructions, but more or less than 1 (and dependent on the state
of the program) for a handful of "branch" instructions. Note also that
`advance()` would not typically be implemented in C as a function; I'm
just showing it like this to emphasize the regular structure.

From this we infer that the basic structure of an interpreter is a
*dictionary* or *map* that associates program symbols with operations.
There is some extra housekeeping like the fetch-execute cycle that
surrounds this dictionary, but this can (hopefully) be handled mostly
automatically, freeing us to concentrate on *symbols* and *operations*.

The symbols could be taken from any finite alphabet, but in Emmental, to
keep things relatively simple, we'll just use the ASCII character set.
(Actually, to be precise, this is the full 8-bit ASCII character set.
Non-printable control characters are allowed, as are characters between
128 and 255, and each has a distinct meaning. But their representations
are not defined.)

The operations can be thought of, abstractly, as functions which
transform program states. Or they can be thought of, concretely, as
segments of code — mini-programs which implement these functions. In the
case of a meta-circular interpreter, these mini-programs would be
written *in the language being interpreted*.

To extend this idea to a *self-modifying* meta-circular interpreter, the
operations can be thought of as functions which transform both program
states *and* interpreter definitions. (Alternatively, the interpreter
definition could be thought of as part of the program state, although I
feel that's a bit gorier a way to look at it, and I prefer the other
view, at least for Emmental.)

In Emmental, most operations leave the interpreter definition unchanged.
However, there is one operation which alters the interpreter definition,
and it is this altered definition that is used to interpret the
remainder of the program.

Emmental Semantics (in Emmental)
--------------------------------

Emmental is essentially a stack-based language. (There's also a queue,
but it's not as central.) All operations implicitly get data from, and
implicitly deposit results back onto, a single stack. For
orthogonality's sake, this stack may contain only ASCII symbols. (And
note that trying to pop an empty stack, or dequeue an empty queue, is an
error that aborts the program.)

Note that because we've established that an interpreter (at least,
insofar as Emmental ever needs to know) is simply a map that takes
symbols to operations, and that operations in Emmental are defined
(meta-circularly) as Emmental programs, we can use the following
notation to describe interpreters:

    % → XYZ+*!
    & → 'ap'ag'ag

That is, the symbol `%`, when encountered in an Emmental program,
indicates an operation that is defined by the Emmental program `XYZ+*!`,
and so forth.

When a main Emmental program begins execution for the first time, it
starts with what's called the *initial Emmental interpreter*. (This
fact, of course, doesn't apply to any further point of execution inside
an Emmental program, or execution of operations defined in Emmental's
MCI, since these would be considered subprograms of a sort. These cases
use whichever interpreter happens to be in force in that point in time.)

The inital Emmental interpreter is defined as follows:

    a → a
    b → b
    c → c
    ...

That is, for every symbol x in the ASCII set, x `→` x.

Doesn't tell us a lot about Emmental's semantics, does it? No. Nothing
at all, really. But remember what I said about needing an external
source of understanding, in order to actually get full value out of an
MCI. And remember the purpose of Emmental's MCI: it is not there so much
to help us understand Emmental, but to allow us to *change* Emmental,
from inside an Emmental program.

And, for all that our description of the initial Emmental interpreter is
unhelpfully tautological, it is not incorrect: the semantics of `a` can
in fact be thought of as being defined by an Emmental program that
consists of only one instruction, `a`. This happy state of affairs comes
about because Emmental is stack-based; the "signature" (the requirements
for the "before" and "after" stacks) of the symbol `a` is the same as
the signature of the program containing the single symbol `a`. No extra
syntax to specify arity and the like is necessary.

Above all, don't panic: we *will* describe what symbols like `a`
actually mean in Emmental, we'll just need to do it in something besides
Emmental. In fact, let's do that right now.

Emmental Semantics (in English)
-------------------------------

### Primitive Arithmetic

`#` pushes the symbol NUL (ASCII 0) onto the stack.

The symbols `0`, `1`, ... `9` pop a symbol from the stack, multiply its
ASCII value by 10 modulo 256, add the value 0, 1, ... 9 (respectively)
to that value modulo 256, and push the resulting symbol back onto the
stack.

The upshot of these 11 operations is that you can push arbitrary symbols
onto the stack by spelling out their ASCII values in decimal. For
example, `#64` pushes a `@` onto the stack.

`+` pops two symbols off the stack, adds together their ASCII values
modulo 256, and pushes the symbol with the resultant ASCII value back
onto the stack.

`-` pops two symbols off the stack, subtracts the ASCII value of the
first popped from the ASCII value of the second popped modulo 256, and
pushes the symbol with the resultant ASCII value back onto the stack.

`~` ("log") pops a symbol from the stack, computes the discrete base-2
logarithm of the ASCII value of that symbol, and pushes the symbol with
the resultant ASCII value back onto the stack. The discrete base-2
logarithm of a number is the floor or integer part of the base-2
logarithm of that number. Alternately, it is the number of the highest
bit position (starting with the LSB = bit position 0) with a bit set
when the number is viewed as binary. Because the base-2 logarithm of the
number 0 itself is undefined, the number 0 is treated as 256 for this
operation; its discrete base-2 logarithm is 8.

### Stack and Queue Manipulation

`^` ("enqueue a copy") pops a symbol off the stack, makes a copy of it,
pushes it back onto the stack, and enqueues the copy onto the queue.

`v` ("dequeue") dequeues a symbol from the queue and pushes it onto the
stack.

Using these operations in combination, one can form "discard",
"duplicate", "swap", and other more advanced stack manipulation
operations. For example, assuming an empty queue and more than two
elements on the stack, "swap" can be accomplished with the code
`^v^-+^^v^v^v-+^v-+^v-+vv`.

Despite this fact, the operation `:` duplicates the top value of the
stack. (Emmental is not an absolutely minimal language; note, for
instance, that it has all ten decimal digits as operations when these
could surely have been defined in terms of only one or two operations.
The reasons for a seperate `:` operation are given below in the section
on Computational Class.)

### I/O

`.` pops a symbol off the stack and sends it to the standard output as
an ASCII symbol.

`,` waits for an ASCII symbol to arrive on standard input, and pushes it
onto the stack.

### Interpreter Modification and Reflection

First let's define what it means to *pop a string* off the stack.
Symbols are popped off the stack until a `;` symbol is found on the
stack. The symbols popped off are considered a string in the reverse
order they were popped; i.e. the last symbol popped is the first symbol
of the string. The `;` symbol is popped off the stack, but is not made
part of the string; it is simply discarded.

Since an Emmental program is a string, popping a program is the same as
popping a string, just that the string is interpreted as a program.

`!` (sometimes called "supplant") pops a symbol, which we call s, off
the stack. Then it pops a program t. It then inserts the association s
`→` t into the interpreter definition. This overwrites whatever mapping
of s might have been in the interpreter definition previously. This new
interpreter definition is used for all subsequent execution (until it is
changed again, of course.)

Note that `!` does *early binding*. That is, the meaning of each symbol
in this program t is the meaning of that symbol *at the time `!` is
executed*. If some subsequent `!` operation later changes the meaning of
one of the symbols that occurs in t, the meaning of t is not changed.
The semantics of t are "captured" or "frozen". This implies, among other
things, that supplanting some symbol z with itself (a program consisting
only of the symbol z) is a no-op, because z's meaning, at the time that
`!` is executed, is invariably z.

`?` (sometimes called "eval") pops a symbol, which we call s, off the
stack. It then executes that symbol (interprets it as an operation) with
the interpreter currently in effect.

Note that `?` does *late binding*. That is, in contrast with `!`, `?`
never "freezes" the semantic definition of the thing that it is
executing. This is true even when `?` occurs in a operation redefinition
(i.e. the program that supplanted some symbol's semantics when an `!`
was executed.) This implies, among other things, that supplanting some
symbol z with the program that consists of instructions that push the
ASCII value of z onto the stack, followed by a `?` instruction, creates
a *cyclic meaning* for z. This is because the z that will be executed by
the `?` will always be the present z, that is, the z that is executing
the `?`.

For convenience's sake, `;` pushes the symbol `;` onto the stack.

All other symbols are no-ops.

Computational Class
-------------------

I believe Emmental is Turing-complete with only the operations that have
been given so far, but I haven't proved it yet. All the elements are
there, and although some of them are somewhat "cramped", they look
viable.

If you want to try thinking about how you could write real programs
(like a universal Turing-machine simulator) in Emmental, you might want
to skip this section, since it contains "spoilers".

Repetition can be accomplished by assigning a symbol a cyclic semantics,
by use of a `?` within a `!`. For example, we can redefine the semantics
of `0` to be `#48?`. This is simply a program that pushes the symbol `0`
onto the stack and executes it with the current interpreter, and, since
`0` has been redefined to mean `#48?` in the current interpreter, this
will loop forever. The entire program to do this to `0` and run the
infinite loop is:

    ;#35#52#56#63#48!0

This technique can also be used to "jump" from one definition to
another, by using `?` to execute some *other* symbol at the end of a
definition (that is, some symbol other than the symbol being defined.)

Conditionals are a little more restrictive. The trick to them is,
strangely, the discrete log operator `~` in combination with the eval
operator `?`. Since `~` maps all symbols into a set of nine symbols, and
`?` executes the symbol on the stack, `~?` will execute one of the
symbols from ASCII 0 (NUL) to ASCII 8 (BS). We can then, for instance,
define NUL to do one thing, define SOH through BEL to do the same as
NUL, and define BS to do some other thing; this essentially
distinguishes between 0 (which executes BS) and every other value (which
executes NUL). Further, we can use this in conjunction with `-` to
compare two values for equality. So, for example, a program which inputs
a character, and outputs Y if the character is M and N otherwise, would
be:

    #59#35#55#56#46#!;##1!;##2!;##3!;##4!;##5!;##6!;##7!#59#35#56#57#46#8!,#77-~?

In case NUL through BS are in use for some reason, we can always add 9
to the result of the log (`~#9+?`) to map the answer onto HT through
DC1. Or, of course, any of a great number of other arithmetical mappings
of our choosing. The most severe constraint is that there be 9 available
symbols to act as "destinations" for our "branch". Even if we never
overwrite one "branch" with another (and we can do that in Emmental!)
and even if we allocate one extra symbol to be the "launch point" of the
branch, we still have room for 25 branches in the ASCII character set.

So these parts look good. If there's a problem, it's with the queue.
Specifically, the problem seems to be the need to know the present size
of the queue in order to do stack housework like "duplicate" and the
subsequent need for "duplicate" to achieve "discard." (Duplicate can be
defined as `^v`, but this only works when the queue is empty. Discard
can be defined as duplicate plus `-+`, but this only works when there
are other elements below the element being discarded. [This last point
is not generally a problem since we can push arbitrary values onto the
stack before any program.])

However, if it turns out that we need "duplicate" or "discard" in order
to write routines that can handle a variable-sized queue — and that
strikes me as likely — then it looks like we have a severe problem.

Here's one way I could try to deal with it. I could say that the queue
is *local* to the operation being defined (or the main program.) Then
you could define `:` to be `^v`, and inside `:`'s definition, the queue
would always initially be empty, so the definition would work.

But... we need the queue to store our global data. For example, if we
are going to simulate a Turing machine, we'd need to use the queue to
store the tape (perhaps "doubled up", with one symbol of each pair
telling us "the next symbol is a simulated tape symbol" or "the next
symbol is some housekeeping value.") We can't store the tape on just one
stack. And, once you are looping in Emmental, you've left the "main
program" forever; you're jumping from definition to definition, and each
has their own queue. At best, you'd need to "dump" the queue onto the
stack each time you switched definitions, and even then you'd need a
loop to do that, and to loop you need to switch definitions. It's a
royal mess.

So here's how I will deal with it. I will add a primitive duplicate
operation, `:`, to Emmental. Proving that Emmental is Turing-complete is
still, then, a challenge, although a doable-seeming challenge. I will
then propose a more formidable challenge: prove that the language formed
by removing the `:` operation from Emmental is Turing-complete.
(Equivalently, prove that the set of Emmental programs that begin with
`;#0#58!` is Turing-complete. The nice thing about Emmental is that you
can always shoot yourself in the foot — until you erase your pistol,
that is.)

And if you *really* like a challenge, try proving that Emmental without
`~` is Turing-complete. I don't think that it is, although it's possible
for it to compute parity, at least (input a symbol, output E if its
ASCII value is even, and O if it's odd. To accomplish this, multiply the
input's ASCII value by 128 by adding 127 copies of it to it; this is
modulo 256, so the only results can be 0 or 128. Define those operations
to print out E and O respectively. But that's as far as I've gotten.)

Discussion
----------

### Design Decisions

I would've liked to have given Emmental a `'` or `"` instruction similar
to Funge's "quote" and "quote-mode" instructions; instructions that
treat one or more of the following symbols in the program literally,
pushing them, as symbols, onto the stack, instead of executing them.
However, such instructions are somewhat problematic, both theoretically
and (for the approach I took implementing Emmental) practically. There
are two ways of thinking about the problems that arise.

One is that the function which implements `'` is given access to the
program text itself, and possibly the position within the program, and
it uses these to extract the "immediate mode" symbol past the `'`. This
information could be available because these pieces of information are
considered extra arguments to the function, or because they are (gorily)
considered part of the overall program state. Either way, this operation
is given a lot of information to work with, and for consistency (since
we want to be all nice and neat and say that all operations have the
same signature so that our dictionary has a nice uniform type,) *all*
operations have access to this information. This is almost too much
information; that is, it is so much that operations don't really *need*
the dictionary. We could just say there is *one* operation, defined by a
function, and that function is given the current symbol and has to
decide what it means through whatever means it likes.

This approach is very powerful, of course, but it's just not the style
that Emmental embodies. (In fact, the idea to view interpreters as
dictionaries was one of the foundational design choices for Emmental, to
the point where I started constructing a "little theory of interpreters
as maps." It really wasn't exploited as much as I think it could have
been. If an interpreter is a map of symbols to strings of symbols, it's
much more tractable than an opaque function would be; you can define all
sorts of operations on them, for example concatenating two interpreters
(for all symbols s in interpreter a and interpreter b, c[s] `→` a[s]b[s]
— that sort of thing,) computing union or intersection of interpreters,
Cartesian product, etc.)

The other way of looking at it is to say that there are in fact
*multiple* meta-circular interpreters available inside Emmental, and
symbols like `'` switch temporarily to an alternate MCI. This alternate
MCI interprets every symbol as "push this symbol", then reinstates the
previous MCI. I like this explication better than the one above — MCIs
begin to look a bit like continuations! — but to do it justice would
take some work. I envision a language where the program has fine control
over which MCI is in effect, possibly by keeping a map from symbols to
MCIs, or maybe even being able to push MCIs onto the stack. This is a
wee bit much for Emmental too though, and perhaps I'll explore these
possibilities in a future language.

### Turing-completeness

You can make the argument that Emmental's way of being Turing-complete
is really nothing new: when you redefine some symbol, you're really just
defining a new function, and when you use `?` to execute that symbol
from within its own definition, you're just making a recursive function
call.

Well, yes, you can make that argument. But it has to do with how you
think about "what is a language", I think. Does a Pascal program
fragment which defines a procedure called `PrintFibonacci` represent
another programming language, one different from Pascal? You could
certainly say that it does — it's the language Pascal where the token
`PrintFibonacci` has some meaning that it doesn't have in Pascal.

In that view, any language where you can define procedures, or
functions, or standard libraries, or the like, is an extensible
language. But even languages where you *can't* define new procedures or
functions is arguably an extensible language. Take some initial
Brainfuck program fragment, for instance. After it executes, it leaves
the Brainfuck tape and while-stack in some state that depends on the
input. Any Brainfuck fragment that executes after that, will execute in
that environment, and that environment is arguably a version of the
language Brainfuck, suitably extended.

You don't normally think of it that way, I bet, but you *could* — and
you would need to, to some degree, to claim that Emmental is "just"
defining new functions. The reason you don't typically look at languages
like this (unless you are very strange) is because it's much more useful
to divide the world into "languages" and "programs." And Emmental *does*
make this division, it just makes it in a slightly different place than
usual.

As far as I'm concerned, if I describe what Emmental does as modifying
the Emmental language via its MCI, and what Emmental actually does is
consistent with the idea of modifying the Emmental language via its MCI,
then what Emmental effectively does is modify the Emmental language via
its MCI. And if it needs to do this in a certain way in order to
simulate a universal Turing machine, then that difference (however
slight) sets it apart from systems where this simulation needs to be
done by defining recursive functions.

Implementation
--------------

`emmental.hs` is a reference interpreter for Emmental written in
Haskell. Run the function `emmental` on a string; you can also run
`debug` on a string to view the state of the program (stack & queue)
during execution. (Note that `debug` is *not* able to show program
states that occur internal to an operation.)

Happy interpreter-redefining!  
Chris Pressey  
Chicago, IL  
November 11, 2007
