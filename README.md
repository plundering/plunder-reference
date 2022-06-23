Plunder Reference Implementation
================================

This is a reference-implementation for the stack of technologies needed
to bootstrap the Plunder system.


Mission Statement
=================

45 years after the release of the Apple II, we have regressed and reneged
on the promise of user empowerment. Users of computers are not in control
of their computation because they do not understand what their computers
are doing, leading to code which is not in the user's interest. This is
true even in the case of Free/Libre Software: Freedom 1 is the freedom to
study and change a program to make it work as you wish, but sufficiently
complex code makes doing either infeasible. Does Firefox or GCC actually
comply with the spirit of Freedom 1?

The code run on user's computers then changes, to add unused features
or to be even more user hostile, and being open source is not a defense
against this. Product designers usually deride any complaints about this
process as "change aversion". But aversion to these changes is correct:
changes are an externally that product managers and designers force on
users, where change often makes the product worse for the user, while
the product manager or engineer can reap the benefits by Demonstrating
Impact to a promotion committee.

Open standards don't entirely help, because they change over time. Large
companies have more motivation and time to spend on bureaucratic standards
committees. Because of the bias towards shipping new features, these
open standards become even more complex, and that complexity acts as a
moat to prevent reimplementation. Will there ever be another complete
web rendering engine after WebKit/Blink? It seems unlikely.

To recover freedom, we must have a system that is radically
understandable. A smart user-programmer must be able to understand the
system from top to bottom. We need a computational formalism that:

-   The low level formalism must be so simple that it is obviously
    correct and that there's no place for misunderstandings or divergent
    behaviour. It should be possible for any programmer to build a
    decent-enough interpreter for the system.

-   The formalism must be directly inspectable, including from inside the
    formalism: if I give you a value, you should be able to inspect it
    to see what it does. Binary artifacts in complex formats are not
    inspectable.

-   The formalism must never change, as it will then be susceptible to
    embraced/extended/extinguished.

-   For every computation, the formalism must have a single
    deterministic answer. Divergent behaviour between implementations
    isn't just a security or privacy problem, it means we can't rely on
    portability between interpreters.

-   The formalism must still be able to handle realistic loads. We can
    not punt on things the mainstream world wants because otherwise they
    won't use this formalism in the first place. Real users have real
    amounts of data, often in the hundreds of gigabytes.

Running the System
==================

You will need to install `lmdb` and Haskell's `stack` to build this.

    $ stack install --fast
    $ sire tests/laws.sire

To understand the system you should start by reading
[laws.sire](tests/laws.sire).  It starts by giving a brief explanation of
all of the technologies involved, and then proceeds to bootstrap the
whole world from scratch: from the low level formalism, it builds up
natural number math, data structure definitions, and even the BLAKE3 hash
function. The rest of this document will give examples using the
environment defined in `tests/laws.sire`, explaining the why and how.

But why? And why not Lisp?
==========================

In a radically understandable system, the user-programmer must be able to
inspect everything. Everything must be a value and there must be no
hidden state. The entirety of the system should be expressed in one data
structure; the entire system should be homoiconic.

When we mention homoiconicity, people ask, "Why not Lisp?" The simple
answer is that most Lisps, in practice, aren't homoiconic:

    $ nix-shell -p chez
    $ petite
    Petite Chez Scheme Version 9.5.8
    Copyright 1984-2022 Cisco Systems, Inc.

    > (car '(a b))
    a
    > (car (lambda (x) (add x 2)))
    Exception in car: #<procedure> is not a pair
    Type (debug) to enter the debugger.
    >

What is this `#<procedure>` thing? Why is a function encoded as something
other than a bunch of cons cells? That is Lisp's claim to fame, no? This
is a real issue and not a nit because this means you can't send all
values across a network interface. I can't write a function value (or
better yet, a partially applied function value), send it across a socket
and have the other side execute it.

Along the same lines, the environment means you have hidden state which
affects how functions execute. This is hidden state:

    > (define (op x) (+ x 5))
    > (define (doit x) (op x))
    > (doit 2)
    7
    > (define (op x) (+ x 10))
    > (doit 2)
    12
    >

We have changed what is essentially a global variable and have changed
how the function executes. What would it mean to serialize `doit` and
sent it across the wire for remote execution, when there's an implicit
dependency on an environment?

Most Lisps are filled with built-ins which are not well specified. For
example, what sort of thing is the `-` function? It's a built in that you
can't inspect and which is implementation defined:

    > -
    #<procedure ->
    > (car -)
    Exception in car: #<procedure -> is not a pair
    Type (debug) to enter the debugger.
    >

Many lisps have other data structures. For example, vectors for memory
locality:

    > `#(1 2 3)
    #(1 2 3)
    > (car `#(1 2 3))
    Exception in car: #(1 2 3) is not a pair
    Type (debug) to enter the debugger.
    >

But once again, the vector doesn't have a canonical form as a list if you
try to inspect them. This vector data type is a primitive and does not
have a representation in the unenriched lambda calculus.

What is the Plunder project? Plunder defines an encoding of the
unenriched lambda calculus on top of binary trees of natural numbers,
where everything in the system is a binary tree value, while still being
performant by allowing what would be built-in primitive functions like
subtract or data structures like vectors with memory locality to have
definitions given in the binary tree formalism.

Show me a demo of why that matters
----------------------------------

Let's say we have a network with multiple nodes...

[TODO: The network stack isn't finished yet, for now, this test uses
multiple local plunder Processes in the same Machine. Once that's
completed, all that changes is the syscalls being used. Nothing is
conceptually different.]

...and you have two different processes that want to talk to each
other. Runtime support should be able to handle passing any value between
processes.

```
= [server parentId selfId arg]
@ st | mkState selfId
| recvFrom st {parentId}
? [loop st from func]
@ ret | func arg
| sendTo st parentId ret
& [st]
| recvFrom st {parentId} loop
```

Since this is the first time you might have seen sire syntax, we'll
comment every line in the example with what it does:

```
; Declare a function `server` which takes three args.
= [server parentId selfId arg]
; @ is a let bind, and | is shorthand for function call.
; This is equivalent to `let st = mkState selfId`
@ st | mkState selfId
; Calls the recvFrom function with st and a "row" (contiguous memory
; vector) with one element: parentId. recvFrom (defined elsewhere) has
; arity of 3. It's third argument is on the next line.
| recvFrom st {parentId}
; Define a named loop lambda which can be referred to later in the
; definition. This is called CPS style from recvFrom once we've received
; a message from parentId.
? [loop st from func]
; This server executes the passed in function against the arguments
@ ret | func arg
; Another function call. This sends the result back to our parent.
| sendTo st parentId ret
; Define an anonymous lambda that can't be referenced. It has one
; argument, st.
& [st]
; This loops.
| recvFrom st {parentId} loop
```

When spawned as a process, this function listens for messages from its
parent, treats the message as a function, and sends the result of running
that function against a constant argument passed in at spawn time.

Since functions are pure values, you can just pass them around, including
over network wires. Including partially applied functions.

Including partially applied functions where the interpreter has a faster
implementation, like `sub` (the equivalent of Scheme's `-` above). You
can pass this value over the wire:

    foldl sub 10000

While the Plunder formalism is "just" binary trees of natural numbers, an
interpreter can use any representation that's a proper bijection to
it. And that bijection can contain higher level concepts like, "This
binary tree value pattern matches to the well known specification of
natural number subtract, just run natural number subtract instead of
performing all the raw lambda term substitutions."

But even when pattern matched by the interpreter, `sub` is still a value
you can print and inspect.

    $ sire tests/laws.sire
    [a ton of output, printing out each value in the standard library]

    > sub
    [sub a b]:=[exec:dec a b]

    > lawBody | pinItem sub
    _/[0 [0 exec-dec 1] 2]

You might complain that focusing so hard on partial application is a
distraction (we disagree, since a lot of functions in Haskell are defined
in terms of smaller functions), so let's focus on data structures.

In the unenriched lambda calculus, index based vectors are partially
applied functions.

    > {1 2 3 4}
    {1 2 3 4}

    > mkRow 4 1 2 3 4
    {1 2 3 4}

This value is also statically pattern matchable by the interpreter and
can be replaced with whatever array like data structure the host language
has instead of doing all the lambda term manipulation. Likewise,
functions that operate on vectors can be statically recognizable so that
they can then operate on that memory optimized structure.

And if an interpreter doesn't implement this pattern matching, it doesn't
break semantics at all.

More Documentation
==================

Specifications can be found in [doc/](doc/), but they are presented
formally and without much explanation.  The "Tours" in
[laws.sire](tests/laws.sire) are much more accessible:

-   [PLUNDER_SPEC.txt](doc/PLUNDER_SPEC.txt)
-   [REX_SPEC.md](doc/REX_SPEC.md)
-   [SIRE_SPEC.md](doc/SIRE_SPEC.md)
-   [MACHINE_SPEC.md](doc/MACHINE_SPEC.md)

This project is a stack of technologies:

Plunder: The Runtime System
---------------------------

Plunder is a lazily evaluated graph-reduction engine built on binary
trees of natural numbers.

Plunder is extremely simple and is permanently frozen.

There is a blessed set of Plunder functions which runtime systems may
recognize and replace with a more efficient implementation.  In this
way, new primitive operations can be introduced without changing the
formal model.

Having a frozen model is important, because we expect Plunder programs
to have uptime in the decades (since Plunder processes are persistent,
they can live through hardware reboots).

Plunder software written today will continue to work forever, and should
be completely portable across all conformant implementations.


Machine: The IO System
----------------------

Plunder "Machines" are an IO system that runs a set of Plunder processes
and give them access to various IO operations.

Plunder machines are persistent, so no external database is needed.
The implementation uses a snapshot-and-event-log system for efficient
persistence.

Processes can pass message around locally, and will soon be able to
communicate over the internet with a public-key based addressing system.


Rex: A parser for the surface syntax.
-------------------------------------

Rex(pressions) are somewhere in between Lisp S-Expressions and Hoon's
Runic Syntax.  Like in Lisp, we parse Rex into a simple generic structure,
and then we interpret that structure into an actual AST.


Sire: A simple bootstrapping language.
--------------------------------------

Starting with only lambdas and macros, we can bootstrap a whole
ecosystem from scratch.  There is no need for "pills", since we can
freeze the bootstrapping language.
