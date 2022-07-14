Building a Persistent REPL
==========================

As an initial application, we should extend the `Sire` demo to support
*remembering* history and the current environment.

I should be able to do something like:

    $ ls -a | grep .sire
    $ sire
    load!!"tests/laws.sire"
    add
    [add a b]:=[exec:3 toNat-a b]
    ^D
    $ ls -a | grep .sire
    .sire
    $ sire
    add
    [add a b]:=[exec:3 toNat-a b]

When Sire is started, it should run a Machine in `$PWD/.sire`.  If one
is already there it should resume it, otherwise it should start a new one.

The Machine should consist of a single database process that tracks
the environment and the input history.  REPL inputs and environment
changes should be recorded by sending updates to this database from a
Host Process.

When a Sire REPL is resumed, the environment and input history should
be loaded from the database.
