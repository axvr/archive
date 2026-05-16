# Safety

Engineering a Safer World.

Security.

Prioritising safety and flexibility over "correctness".


Systems are dynamic.


Aim for system safety over component reliability.  If a single component
failure (e.g. integer overflow) brings down your system, it is poorly designed.

No amount of testing or verification can prove a system will behave correctly.

A system achieves safety through enforcing the necessary constraints to prevent
unsafe behaviour.

> Bug: a misnomer.

> Static types eliminate a whole class of bugs.

Well, what is this "class of bugs" let's define it.  Static type systems do not
help protect against system issues and can actually lead to system instability
by reducing the dynamism of the system and increasing the complexity due to the
loss of dynamism.

You feel better, but it is not more reliable.

Networks are dynamic.  The internet would be useless if it were static.

Build tools for effective control over the dynamism and utilise them throughout
the lanuage and system.
