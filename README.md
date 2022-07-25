Plunder
=======

Plunder is a new programming model that makes it possible to have
programs that "run forever", eliminating the need for a separate storage
abstraction.  Processes survive hardware restarts, and can be moved
between physical machines.

Plunder is a purely-functional virtual machine in which new code can be
introduced dynamically.  Code can construct code.  This gives long-running
processes a way to upgrade themselves.

This combination of these features creates a unique computing environment,
where everything lives inside the VM.  Code inside the VM has zero
dependency on the specifics of the host operating system.  Even the
compiler toolchain, and development environment live within the VM.

A network abstraction is also included that uses public keys as
endpoints.  Any object can be moved across the network, including code.
NAT traversal, name resolution, and message transport are all built
into the abstraction.  A BitTorrent-like system is also included so that
larger datasets can be hosted by nodes with limited bandwidth.

This lets networked services be created as simple programs.
No dependencies, no databases, no deployments.  These services can be
transparently hosted on residential networks.  Self-upgrading peer-to-peer
applications can also be built easily.

Processes that "run forever" are possible because evaluation is fully
deterministic and because process state is a simple, serializable value.
This lets Plunder use the snapshot-and-event-log approach to keep program
state synced to disk.

Services can be easily migrated between hardware.  You can run a service
on your laptop as you develop it, and then move it to a cloud host to
get more bandwidth temporarily.  You can later pull it from the cloud
host, and move it onto a server running on a smartphone.  This kind of
migration is easy, safe,  and requires no client-side changes.

Most modern applications require a team of engineers, cloud hosting, and
constant maintenance churn.  Because of this, any serious application
requires significant investment and must eventually find some way to
extract money from its users.  This has created a world where finance
and corporate players have total power over all of computing.

Our hope is that Plunder will make it practical for motivated
individuals to build these types of applications themselves, resulting
in a proliferation of networks and software systems free of centralized
control.  People will be able to communicate, socialize, and organize
without invasive corporate oversight.


Plunder and Urbit
=================

Plunder is directly inspired by Urbit, and is reaching for more-or-less
identical goals but with different foundations.  Urbit has a number of
limitations that motivated this work:

-   Urbit has several significant performance limitations stemming from
    core architectural problems.

    Compute, networking, and storage all have limitations; Urbit
    cannot work with significant amounts of data.  For example, hosting
    something like a YouTube channel within Urbit is far outside the
    ream of possibility.

-   In practice, old code doesn't work on new implementations.  This is
    true for several reasons, I wont get into it here.

    Changes to the core network protocols break old code.  In practice,
    the entire architecture is very dependant on accepting over-the-air
    updates from Tlon, which makes Urbit a de-facto centralized system.

-   Nock and Hoon are tightly coupled.  In practice, building your own
    language that compiles to Nock is not even remotely possible.
    This is because of the design of the intrinsics system ("jets")
    and because the implementation is only practical for very specific
    patterns of code.

    What is the point of having this tiny core language if it is de-facto
    tightly coupled to (a specific version of) a complex high-level
    language?

    Plunder, in contrast, can support any purely functional programming
    language.

-   "Urbit is not a darknet project".  In practice that means that you
    depend on cloud hosing, or your home IP becomes public.  Plunder *is*
    a darknet project.

    Plunder will run primarily over I2P.  But will eventually support
    a wide range of networking transports.  You should be able to use
    a relay server or direct UDP.  We hope that it will eventually
    be possible to even use use GMail, Zoom, and Facebook Messenger
    as transports.


A Quick Tour of Fan
===================

Fan is Plunder's version of Nock.  Fan is a pure functional evaluation
engine with universal introspection.

You could think of Fan as "The Ultimate Lisp", since it is truly
homoiconic and absolutely minimal.  Even the internal representation
for functions is just a normal data structure.

A Fan value is either a natural number, a function, or a partial
application of a function.

```haskell
data Nat = Z | S Nat

data Fan
    = F Nat Nat Fan
    | A Fan Fan
    | N Nat
```

Naturals can be used as functions, with the first four numbers
being the four primitive operations.

    - 0: Construct a function (The `F` constructor)
    - 1: Pattern match on a value.
    - 2: Pattern match on a natural.
    - 3: Increment a number (the `S` constructor)

Fan functions are *super combinators*.  They take a certain number
of arguments, and code refers to arguments by argument-number.
There are no closures, but you can compile lambda expressions to this
by lambda-lifting.

Functions have names, but the names aren't important.  They're just used
to increase legibility and speed up-intrinsics matching.

Functions have let-bindings and support self-reference.

Function bodies look something like:

```haskell
data Exp
    = Sel          --  Self-reference.
    | Ref Nat      --  Reference argument #n
    | Cns Fan      --  Return constant value
    | Kal Exp Exp  --  Function application
    | Let Exp Exp  --  Let binding (extends the argument-list)
```

Plunder does not directly support mutual recursion.  Because references
to other functions are by direct value-reference, mutual recursion would
require cyclic data which cannot be serialized.  This can be resolved in
the source language by translating mutual recursion into single recursion.

To keep the data model simple, function bodies are encoded using partial
applications of the primitive operations.

    (0 x y) == Kal x y
    (1 v b) == Let v b
    (2 x)   == Cns x
    0       == Sel
    n       == Ref (n-1)

These explanation papers over a lot of details, but the following 60
lines of Haskell is a complete implementation:

```haskell
{-# LANGUAGE LambdaCase, BangPatterns #-}

module Fan where

import Numeric.Natural

data Fan
    = F !Natural !Natural !Fan
    | A Fan Fan
    | N !Natural

force :: Fan -> Fan
force (A f x) = A (force f) (force x)
force x@F{}   = x
force x@N{}   = x

valNat (N n) = n
valNat _     = 0

arity :: Fan -> Integer
arity (F n a b) = fromIntegral a
arity (A f x)   = arity f - 1
arity (N 0)     = 3
arity (N 1)     = 4
arity (N 2)     = 3
arity (N _)     = 1

(%) :: Fan -> Fan -> Fan
(%) f x | arity f == 1 = eval [x] f
(%) f x | otherwise    = A f x

eval :: [Fan] -> Fan -> Fan
eval args f@(F _ _ b) = subst (1 + fromIntegral (length args)) (f:args) b
eval args (A f x)     = eval (x:args) f
eval args (N i)       = primOp i args

subst :: Natural -> [Fan] -> Fan -> Fan
subst r xs = \case
    N n | n<r       -> xs !! fromIntegral n
    A (A (N 0) f) x -> subst r xs f % subst r xs x
    A (A (N 1) v) b -> let xs'   = xs <> [bound]
                           bound = subst (r+1) xs' v
                       in subst (r+1) xs' b
    A (N 2) v       -> v
    v               -> v

mkFun :: Natural -> Natural -> Fan -> Fan
mkFun !n 0 !b = res where res = subst 1 [res] b
mkFun !n a !b = F n a b

primOp :: Natural -> [Fan] -> Fan
primOp 0 [n, a, b]          = mkFun (valNat n) (valNat a) (force b)
primOp 1 [f, _, _, F n a b] = f % N n % N a % b
primOp 1 [_, a, _, A f x  ] = a % f % x
primOp 1 [_, _, n, v      ] = n % v
primOp 2 [z, p, x]          = if 0==valNat x then z else p % N(valNat x-1)
primOp 3 [x]                = N (valNat x + 1)
primOp _ _                  = N 0
```

Here also, is a 33 line specification:

```
Every tree is either a fun x:{n a b}, an app x:(f g), or a nat x:@.

Evaluate by reducing repeatedly with (->).  Reduce outer trees first.

(f g x) := ((f g) x)

NTH(v,n,fb) is the nth element of the array v (or fb if out of bounds)
LEN(v)      is the length of the array v.
PUSH(v,x)   is the array v with x appended.

NAT(x) = if x:@ then x else 0

RUN(r,(0 f x)) = (RUN(r,f) RUN(r,x))
RUN(r,(1 v k)) = RUN(s,k) where s = PUSH(r,RUN(s,v))
RUN(r,(2 v))   = v
RUN(r,v:@)     = NTH(v,n,v)
RUN(r,x)       = x

v:@                          =>  ARITY(v)=NTH([3 4 3], v, 1)
v:(f _)                      =>  ARITY(v)=(ARITY(f)-1)
v:{n _ _}                    =>  ARITY(v)=n
ARITY(v)>0                   =>  WHNF(v)
v:@                          =>  NF(v)
v:(f g) WHNF(v) NF(f) NF(g)  =>  NF(v)
v:{n a b}                    =>  NF(v)
WHNF(n) WHNF(t) NF(b)        =>  (0 n a b)    -> {NAT(n) NAT(a) b}
v:{n a b}                    =>  (1 f a n v)  -> (f n a b)
v:(f x) WHNF(v)              =>  (1 f a n v)  -> (a f x)
v:@                          =>  (1 f a n v)  -> (n v)
WHNF(v) w=NAT(v)             =>  (2 z p v)    -> if w=0 then z else (p (w-1))
WHNF(x)                      =>  (3 x)        -> NAT(x)+1
f:@ f>3                      =>  (f x)        -> 0
n=LEN([..])                  =>  ({n a b} ..) -> RUN([{n a b} ..],b)
```


### Intrinsics

We need to freeze this representation to avoid destroying our "immortal
processes", so we can't extend this by adding new operations.

We can, however, add new "intrinsics".  As long as the behavior of
every intrinsic exactly matches the code it replaced, the language can
be extended transparently.

In the reference implementation above, `mkFun` can be extended to
examine the function to see if it is an intrinsic.  For example, we could
hypethetically define the `add` intrinsic to match on any function with
the following value.

    toNat = (0 "toNat" 1
             (0 (2 0 3) 1))

    exec = (0 "exec" 3
            (0 (0 (0 (2 2) 2)
                (0 (0 0 1) (0 1 2)))
              3))

    add = (0 "add" 2
           (0 (0 (exec 3) (0 toNat 1)) 2))

For practical reasons, this isn't exactly the approach that's used in
practice, but it demonstrates the general idea.

The practical problem is that intrinsic matching needs to be fast,
but this approach requires a full traversal of the function to check
for equality.  This is solved by "Pins" which are a way to intern values.

This approach of having functions with specific names and shapes can
also be used to introduce new data types.  For example, arrays are
represented as:

    ((0 0 5 0) 1 2 3 4) = [1 2 3 4]

To achieve actual, in-practice portability across all implementations,
these intrinsics must still be standardized and supported by all
implementations.  The formal behavior doesn't depend on any intrinsics,
but extreme performance differences across implementations still breaks
de-facto portability.

However, the important thing here is that the *DATA MODEL* never changes.
This way, introducing new intrinsics does not affect code in running
processes.


This Codebase
=============

This codebase is a reference-implementation for the stack of technologies
needed to bootstrap the Plunder system.

This is still a work-in-progress. The virtual machine itself is fully
working, but the persistence layer and IO system are not fully nailed
down yet.

This implementation is surprisingly capable, given it's extremely
straightforward implementation.  The code here is designed to provide
a clear demonstration of the ideas and to provide a pleasant way to
play around with the system.  It is however, far slower than serious
implementations.

There is another, unreleased, implementation which is written in C and
had a naive translation from Plunder laws to machine code.  That code
has bit-rotted, but it was several orders of magnitude faster than this.
Another several orders of magnitude should be possible with actual
code optimization.


### Running the System

You will need to install `liblmdb` and Haskell's `stack` in order to
build this.

    $ stack install --fast
    $ sire tests/laws.sire

To understand the system you should start by reading
[laws.sire](tests/laws.sire).  It starts by giving a brief explanation of
all of the technologies involved, and then proceeds to bootstrap the
whole world from scratch: from the low level formalism, it builds up
natural number math, data structure definitions, and even the BLAKE3 hash
function. The rest of this document will give examples using the
environment defined in `tests/laws.sire`, explaining the why and how.


The Plunder System
==================

Specifications can be found in [doc/](doc/), but they are presented
formally and without much explanation.  The "Tours" in
[laws.sire](tests/laws.sire) are much more accessible:

-   [PLUNDER_SPEC.txt](doc/PLUNDER_SPEC.txt)
-   [REX_SPEC.md](doc/REX_SPEC.md)
-   [SIRE_SPEC.md](doc/SIRE_SPEC.md)
-   [MACHINE_SPEC.md](doc/MACHINE_SPEC.md)

This project is a stack of technologies:


### Fan: The Virtual Machine

Already discussed.


### Machine: The IO System

Plunder "Machines" are an IO system that runs a set of Plunder processes
and give them access to various IO operations.

Plunder machines are persistent, so no external database is needed.
The implementation uses a snapshot-and-event-log system for efficient
persistence.

Processes can pass message around locally, and will soon be able to
communicate over the internet with a public-key based addressing system.


### Rex: An R-Expression Parser

R-expressions are somewhere in between Lisp S-Expressions and Hoon's
Runic Syntax.  Like in Lisp, we first parse Rex into a simple generic
structure, and then we interpret that structure into an actual AST.


### Sire: A Bootstrapping Language

Sire is an ultra-minimalist language used to bootstrap a Plunder software
ecosystem.  The behavior of Sire is fully specified.

In most systems, the toolchain is distributed as a binary.  Even Urbit,
which has an architecture similar to plunder, requires "pills" which
are blobs of Nock code for bootstrapping.  Plunder, in contrast, does
not use binary blobs.

Binary blobs are difficult to audit and annoying to distribute.
So instead, Plunder runtimes are required to include a Sire compiler,
and the initial toolchain is bootstrapped from that.

Sire has almost no features.  It has natural numbers, lambdas, and macros.
Starting from only that, everything else is bootstrapped: Multiplication
and strings, vectors and dictionaries, serialization and hashing, etc.


Against Software Complexity
===========================

45 years after the release of the Apple II, we have regressed and reneged
on the promise of user empowerment. Users of computers are not in control
of their computation because they do not understand what their computers
are doing, leading to code which is not in the user's interest. This
is true even in the case of Free/Libre Software: "Freedom 1" is the
freedom to study and change a program to make it work as you wish, but
sufficiently complex code makes doing either infeasible. Does Firefox
or GCC actually comply with the spirit of Freedom 1?

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
    embrace/extend/extinguish.

-   For every computation, the formalism must have a single
    deterministic answer. Divergent behaviour between implementations
    isn't just a security or privacy problem, it means we can't rely on
    portability between interpreters.

-   The formalism must still be able to handle realistic loads. We can
    not punt on things the mainstream world wants because otherwise they
    won't use this formalism in the first place. Real users have real
    amounts of data, often in the hundreds of gigabytes.

"Couldn't you use Lisp for this?"
=================================

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

What is the Plunder project? Plunder is a simple functional VM that
encodes function application with arities on top of binary trees of
natural numbers. You can compile the lambda calculus to Plunder just by
lambda lifting. Everything in the system is a binary tree value, while
still being performant by allowing what would be built-in primitive
functions like subtract or data structures like vectors with memory
locality to have definitions given in the binary tree formalism.

### Why does this matter?

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
performing all the raw function application."

But even when pattern matched by the interpreter, `sub` is still a value
you can print and inspect. Without delving into the function
representation, you can see that we're able to pull apart `sub` into its
component binary tree values with the standard `car`, `cdr`, `cadr` etc.

    $ sire tests/laws.sire
    [a ton of output, printing out each value in the standard library]
    ;
    ; ==== Sire REPL ====
    ;
    ; Since input is multi-line, there is currently no input-prompt.
    ; Just type away!
    ;

    sub
    [sub a b]:=[exec:dec a b]

    car sub
    _/4

    cdr sub
    _/(sub a b ? exec:dec a b)

    cadr sub
    _/[0 %sub 2]

You can also construct the raw function value by putting its parts back
together:

    ((car sub) (cdr sub))
    [sub a b]:=[exec:dec a b]

    sub 10 2
    _/8

    ((car sub) (cdr sub)) 10 3
    _/7

    ((car sub) ((cadr sub) (cddr sub))) 10 4
    _/6

    ((car sub) ((cadr sub) ((caddr sub) (cdddr sub)))) 10 5
    _/5

You might complain that focusing so hard on partial application is a
distraction (we disagree, since a lot of functions in Haskell are defined
in terms of smaller functions), so let's focus on data structures.

If you just had function application and a system which pattern matched
binary trees, how would you encode vectors? Well, you could encode them as
partially applied functions. This would be so common, you probably want
literal syntax for it:

    {1 2 3 4}
    _/{1 2 3 4}

But what is this value? It's a well known constructor function which can
be pattern matched by the interpreter. We give it the size of the vector.

    mkRow 4
    _/R4

    mkRow 4 1 2 3 4
    _/{1 2 3 4}

    mkRow 4 1 2 3 4 0
    _/R4

This value can be statically pattern matched by the interpreter and can
be replaced with whatever array like data structure the host language has
instead of doing all the function application. Likewise, functions that
operate on vectors can be statically recognized so that they can then
operate on that memory optimized structure. But if an interpreter doesn't
implement pattern matching, it doesn't break semantics at all because it
can just evaluate the raw binary tree.

Using partial function application, you can build data structures that
are backed by optimized representations. And since they're just function
application values, you can serialize them or send these values over a
wire, even between interpreters written in different languages with
different internal structures.

<!---
Local Variables:
fill-column: 73
End:
-->
