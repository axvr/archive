# Porcelina

## Plan

Write applications for Porcelina in itself.

Based on Smalltalk, Common Lisp, Clojure, Scheme, Erlang, and more.

Option to run in "headless" mode (without GUI).

1-based indexing.

Full Unicode support.

Everything is an object/actor (original idea behind objects).

- Systems have processes.
- Objects have methods.

Processes have inputs and outputs.  Complex systems are built out of the
processes of simpler systems.

Based on the real world and microbiology.

Systems over data.  Only systems and processes.  This solves the data meaning
problem.

(Separate specialised system for **every** piece of data?)

(Inherit specific processes from another system?)

Systems run processes on themselves and arguments to modify internal state and
return a new system.  (The reference pointer updates to refer to the new
system?  Or do something closer to what the Actor model does.)

Process is an operation to perform on arguments and/or the parent system.

x + 1.

System `x` runs the `+` process with argument `1` (another system), which
returns a new system.

Polymorphism.

Processes are just interacting systems.

Processes vs messages?

_Systems (objects) have processes (methods) which are triggered by messages
(async method calls)._

Objects/systems are actors.

Immutable objects?  No state change (behaviours/processes).  Returned new
object.  Assign label to new state.  (See Rich Hickey's "Are we there yet?"
talk.)

Clojure atoms?  Smalltalk's `become:`

Functions and dumb data cannot scale cleanly on networked machines.
Object/actor hybrids might be able to.

Based on Alan Kay's original concept of objects, Joe Armstrong's Erlang, Carl
Hewitt's Actor model, and Rich Hickey's Clojure.

True concurrency.  An object lives on it's own thread or separate machine.
Porcelina must make it as easy to send a message to a remote object as it is
a local object.  Must not see the hardware boundary.

The Porcelina VM should be portable.  Design code to fit it's APIs and
environment.  Spread to other platforms, creating an interactive, live
cross-platform and concurrent application development environment.  If it is
attractive enough it'll help with the adoption of massively-parallel CPU
architectures.

Lateral and bottom up disruption.  _Embrace extend, extinguish._

Native feeling?  Or a web-like disregard of UI integration.  Needs custom VM
kernels per-host system.

Web replacement.  Connect to "headless" VM running on an external server from
your VM which can act as a graph-DB.  New DB ideas (sending queries).
Image-based.

- Simple.
- Performance not critical at first (good kernel and VM interfacing could allow
  the kernel to handle more stuff more efficiently).
- New ways of thinking about programming.

Porcelina application can communicate with each other via the VM (including VMs
running on networked machines).

How to handle dependencies?

Share/move stateful objects between VMs?

> In a good object-oriented system it is difficult to find out where the
> hardware stops and the software begins.
>
> — Alan Kay (OOP seminar - VPRI 0246)

Object caching.

Wrappers for existing codecs and encodings.  E.g. PDFs, MP3, PNG, etc.

VM should be mostly written in itself (like Squeak).  Alan Kay said that Dan
Ingalls was able to do graphics in very little code (see Lively Web).  Also see
Nile (STEPS).  Also see Display PostScript and NeWS.


## Time

Use TAI over UTC.  (DJB's Libtai.)

Cannot encounter leap seconds in a truly concurrent/asynchronous system.  The
VM kernel should handle time, not the CPU.

Persistent time model similar to slippage time model?

Rich Hickey's task vs Alan Kay's talk.  Both bring up very similar concepts.
Object mutation results in a new state which then inherits the name of the
previous?

Cannot stop time in a real concurrent/async system.  Use a percieving model.
Once a previous state is stale enough garbage collect.


## Security

Protect against arbitrary code execution harming the system.

Distinction between system/core code and non-system code?

Non-system code has to request access to modify or talk to system objects.
This can be achieved using "fake interface" objects/proxies.  Which are the
only objects which can access the particular resources and first ask the user
for permission.


## Similar examples

- The WWW and JS.
- HyperCard.
- Lively Web/Kernel.
- Smalltalk.
- Common Lisp.
- Erlang.
- Squeak + Pharo.


## Planar reduction

This is a problem regarding what happens if a massively parallel processor is
able to expand into another external processor, but that processor suddenly
disapears.  The computation space has unexpectedly reduced and now the computer
is missing parts of it's program.

How to safely detach peripherals.


## Network communication

Phantom objects.

Send over dependencies (and classes, etc.) if they are missing from the
destination VM.  Use protocols.  If an object on the destination VM already
implements that protocol, use that one instead.  This will allow setting
default applications (e.g. document viewers, media players, etc.).

How to handle authority?  Servers, updates, etc.

Versioned objects?  System objects?


## Other stuff

- [Syntax](syntax)
