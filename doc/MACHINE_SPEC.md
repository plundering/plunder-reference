Machine
-------

In the plunder VM, a "Machine" is a set of running processes.

Some processes are "logged", meaning that they they will survive a
restart because their state is always persisted to disk.  These are
sometimes referred to as "Database Processes".

Some processes are "unlogged", meaning that their state is not kept on
disk.  These processes will just disappear completely if the VM restarts.

Persistence uses a snapshot-and-event-log model, with all logged
processes sharing a single event log.

Processes are just Plunder values.  A formal snapshot of all state in a
machine is (Array Val).

The runtime doesn't keep any sort of "hidden state" about processes: the
entirety of the system should be resumable from an (Array Val). It
doesn't have a permission system. Everything is controlled directly by
the plunder-code of the process.

For example, we have `sleep_until_timestamp`, not
`sleep_for_microseconds`.  If we instead had the latter, it would be a
problem: the starting-time is not a part of the formal state.

Because of that, processes provide a set of "claims" as a part of their
state.  "I am Local Address 5",  "I have the public identity $key", etc.

Processes interact with the world by making requests to the runtime
system and getting responses.  The set of active requests is provided
to the runtime as a part of the process state (again, "no hidden state").

Process
-------

A Process is a partially applied plunder function. Since you can't put
state outside of a process's noun, state that you want to communicate
to the runtime is placed as the first and second function arguments. The
process state is:

    = Exe
    > Row Claim
    > Row Request
    > ResponseIndex
    > ResponseBody
    > Exe

The king expects this function to be partially applied with the claim
table and the requests table.

    [Exe Row|Claim Row|Request]

Claims
------

Processes do not have identities (which would be state outside of the
process's noun), instead they "claim" identities.

A claim is an extensible sum-type of possible identity information for
a process. Currently, only the following claim is implemented:

```
{0 Nat} ; local_address
```

`local_address` is simply a non-unique process id number. It is non-unique
because nothing in the formalism could force it to be unique and we'll
discuss the implications in a future section.

We intend to implement:

```
{1 Nat} ; local_display_name
{2 Nat} ; ed25519_blake3_private_key
```

Where `local_display_name` is meant for a process browser and
`ed25519_blake3_private_key` is meant for inter-machine communication.

Requests
--------

A plunder process tells the interpreter how it wants to be called
by interpreter by sharing a row of requests, which will be listed in
the next section. This row is implicitly numbered, so that the zeroth
element is Request 0, the first element is Request 1, and so forth. An
element of the Natural 0 is interpreted as an empty placeholder, where
a request previously was.

The runtime calls the process with a Response to a Request. If the
runtime is responding to the zeroth request, it applies Nat 0 as the
first argument and the body of the Response as the second.

A process that has an empty request table is dead, as it will never
be called. When you create a process, it must have a request table or
otherwise it will never be live in the first place.

The System-Call Table
---------------------

Elements of the request table must be of one of the following
value-shapes, anything else will simply be ignored.

```
1                 -> IO Nat       ; local_next
{2 Exe}           -> IO {}        ; local_fork_heed
{3 Exe}           -> IO {}        ; local_fork_omit
{4 Nat}           -> IO Why       ; local_bury
{5 Nat Val}       -> IO {}        ; local_kill
{6 Nat Nat Val}   -> IO {}        ; local_send
{7 Nat (Row Nat)} -> IO {Nat Val} ; local_recv
{8 Wen}           -> IO Wen       ; wait
9                 -> IO Wen       ; when
10                -> IO Bar       ; rand
```

#### 1: local_next

`local_next` requests a non-sequential, random `local_address` that
no live process is claiming. A check is performed to ensure that the
returned number is not a number previously returned by `local_next`
that hasn't been claimed by a process yet.

The response is always a natural number.

#### {2 fun/Exe}: local_fork_heed

`local_fork_heed` creates a logged process with the passed in plunder
function. It returns a response after the process has been created.

The response is always the value 0.

#### {3 fun/Exe}: local_fork_omit

`local_fork_omit` creates an unlogged process with the passed in plunder
function. It returns a response after the process has been created.

The response is always the value 0.

#### {4 addr/Nat}: local_bury

`local_bury` monitors when an address claim is no longer made by any
process. When the final process which claimed addr terminates, this
request is responded to with a Why code. A Why is one of:

```
(0 Val) -- The thread was killed by another thread.
1       -- The process shut down (no more requests)
2       -- I, your king, just started up, and I don't see this key.
3       -- The key was simply released by a process (that didn't die yet).
4       -- The process was terminated because of memory exhaustion.
5       -- The process was terminated because it took too long.
```

#### {5 addr/Nat reason/Val}: local_kill

`local_kill` terminates every process which has a claim of the passed
in local address. Every bury listening to this address will receive a
response with the reason passed in.

The response is always the value 0.

#### {6 src/Addr dst/Addr val/Val}: local_send

`local_send` sends a noun from an address (that this process claims)
to another address. Once a process claiming the destination address
receives it, the request is responded to.

The response is always the value 0.

#### {7 dst/Addr srcs/(Row addr)}: local_recv

`local_recv` accepts a message on an address (that this process claims)
which an optional white-list of processes we will accept message from.

The response is `{from/Addr body/Val}`.

#### {8 wen/Wen}: wait

`wait` receives a Response after the requested timestamp.

The response is a natural representing the current time at execution.

All timestamps are the time since January 1st, 1970 in micro-seconds.

#### 9: when

`when` receives a Response which is the current time. Unlike `wait`,
it doesn't delay into the future and can be used immediately.

#### 10: rand

`rand` receives a Response which is 64 bits of cryptographic quality
entropy, represented as a Bar.
