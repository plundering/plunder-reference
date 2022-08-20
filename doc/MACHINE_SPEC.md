Machine
-------

In the plunder VM, a "Machine" is a set of running processes. All
processes survive a restart because their state is always persisted to
disk.

Persistence uses a snapshot-and-event-log model, with all logged
processes sharing a single event log.

Processes are just Plunder values.  A formal snapshot of all state in a
machine is (Map Nat Val), where Nat is an assigned process id.

The runtime doesn't keep any sort of "hidden state" about processes: the
entirety of the system should be resumable from a (Map Nat Val). It
doesn't have a permission system. Everything is controlled directly by
the plunder-code of the process.

Processes interact with the world by making requests to the runtime
system and getting responses.  The set of active requests is provided to
the runtime as a part of the process state (again, "no hidden state").

Process
-------

A Process is a partially applied plunder function. Since you can't put
state outside of a process's noun, state that you want to communicate to
the runtime is placed as the first function argument. The process state
is:

    = Exe
    > Row Request
    > ResponseIndex
    > ResponseBody
    > Exe

The king expects this function to be partially applied with the requests
table.

    [Exe Row|Request]

Since Processes have their ids assigned by the runtime, every Process
starts with a forking function to receive it's pid:

    = Fork
    > Nat
    > Exe

Requests
--------

A plunder process tells the interpreter how it wants to be called by
interpreter by sharing a row of requests, which will be listed in the
next section. This row is implicitly numbered, so that the zeroth element
is Request 1, the first element is Request 2, and so forth. An element of
the Natural 0 is interpreted as an empty placeholder, where a request
previously was. Rows Request numbers are index based 1 because each
process accepts incoming messages from other processes with
Request 0. Message receives being implicit makes queuing easier for
implementers.

The runtime calls the process with a Response to a Request. If the
runtime is responding to the zeroth request, it applies Nat 0 as the
first argument and the body of the Response as the second.

A process that has an empty request table is not dead, as it can still
receive inbound messages on Request 0. But returning something other than
a Row marks the signals that the process has died.

The System-Call Table
---------------------

Elements of the request table must be of one of the following
value-shapes, anything else will simply be ignored.

There are three system calls, in addition to the implicit receive system
call which is always Request 0. System calls are frozen. Their behaviour
must be simple enough to specify. Once specified, their behaviour may
never change.

```
; System Call Table

; explicit
[0 Nat Val]  => IO Val  ; "call"
[1 Nat Val]  -> IO ()   ; "send"
[2 Fork]     -> IO Nat  ; "fork"

; implicit
recv         -> IO [Nat Val] ; "recv"

; -> implies not blocked on event log persistence, => implies blocked.
```

We have ordered the three explicit system calls from what we believe will
be the most common to the least common, since in `jar` serialization
format, 0 is smaller than 1 which is smaller than 2. However, we'll
introduce all four system calls out of order, as `0` has the most to
explain.

#### 2: fork

```
[2 Fork]     -> IO Nat  ; "fork"
```

The `fork` Request is easy: the Request has a forking function as
previously described, creates the process, and receives a Response with
the assigned process id.

#### 1: send

```
[1 pid/Nat payload/Val]  -> IO ()   ; "send"
```

The `send` Request sends `payload` to the Process with `pid`. The Request
receives a Response once the Process with `pid` has executed a Response 0
to the implicit recv with `payloda`.

#### implicit recv

```
recv         -> IO [Nat Val] ; "recv"
```

Being implicit, the `recv` Request has no actual Request arguments. It
receives a Response with the src of the message and the payload of the
message.

#### 0: call

The call request is the only way to receive information from the outside.

[TBD]

<!---
Local Variables:
fill-column: 73
End:
-->
