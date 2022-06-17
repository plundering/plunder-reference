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


Running and Understanding the System
====================================

You will need to install `lmdb` and Haskell's `stack` to build this.

    $ stack install --fast
    $ sire tests/laws.sire

To understand the system you should start by reading
[laws.sire](tests/laws.sire).  It starts by giving a brief explanation
of all of the technologies involved, and then proceeds to bootstrap the
whole world from scratch.

Specifications can be found in [doc/](doc/), but they are
presented formally and without much explanation.  The "Tours" in
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
