Plunder Reference Implementation
================================

This is a reference-implementation for the stack of technologies needed
to bootstrap the Plunder system.

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
