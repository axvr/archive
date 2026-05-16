# Dynamism

___Last updated: 2024-10-27___

Most commonly programmers tend to think of dynamic and static in terms of type
systems, but it extends far further than that.

It's about the ability for your program to change over time.

You can encode a surprising amount into a type system, e.g. lifetimes in Rust,
but this makes the program more difficult to change.  Ada has probably the
greatest level of static-ness.

Type systems are difficult to build and learn.

I think as an industry we far to readily jump to making our programs more
static.  Consider type systems, RPC mechanisms, overuse and misuse of tests.

The unfortunate reality is that the real world of complex interconnecting
systems doesn't reflect that static view.

The world is not static.  It is ever changing.  Systems need dynamism to be
successful and evolve.  Our tools suck at managing and presenting this
dynamism.  Most programming language research is focused on type systems and
static views of the world because these map more nicely to mathematical proofs
and modelling.

There are good reasons that the internet protocols are not typed.  All
information is conveyed in-band.  Imagine how inflexible the Web would be if it
were built on gRPC and Protocol Buffers.

This maps to many things.  Docker freezes an application in time.  Our
languages are a deploy once, shutdown and deploy new bundle model.  Names are
removed from runtime,  You can't connect, inspect and debug a running
application.  The program is frozen in time.

Our tooling lacks essential integration into the systems we build.

Observability, logging, profiling and benchmarking are left as afterthoughts in
language design.  As is the deployment story, both CI/CD and the
infrastructure.

Hot reloading as implemented in many languages is a weak implementation of hot
reloading.

Runtime tangibility.  Ask the program questions at runtime.  Test your code as
you write it.

Frameworks vs. flexibility.  General purpose languages are not needed anymore.

Tests and types do not prove correctness.  Thet do nothing to prove the safety
or correctness of a system.

Tests are not a requirement for good quality code, but are a tool to reach it.
However those tests must be of high quality otherwise they cause the inverse.
Test suite.

Industry is full of potentially bad ideas.  Build and DevOps tools complicate
things.  Linters, formatters and style checkers only needed because we write
and store source code as plain text.  The "I" in IDE is mostly meaningless
today.  LSPs, Treesitter and other existing tools are not sufficient.  AI
coding tools are terrible for the both beginner and experienced developers.
Wastes time, produces bugs, hinders learning and exploration.  Ties you into
using standard (but rubbish) patterns.  The average software developer is not
good at software development.

The software world is one of thought.  There are no physical restrictions
preventing the systems we build from getting too complex.  Discussing details
such as type systems is mostly opinion based as 99% of the industry has never
considered it deeply.  We readily overcomplicate things based purely on
opinions.  E.g. most sofrware systems should absolutely be monolithic, yet
microservices are eeverywhere.  Do not trust anyone to design a distributed
system that is not terrified of distributed systems.

It's a pop culture.  We're focused on the latest trendy thing.

Performance benchmarks are useless metrics.  The speed of a programming
language is not important, so long as it is fast enough.  Usually you can just
buy or rent a better machine.

AWS lambdas are a terrible idea.  Or maybe a good idea, that is poorly
implemented.

Anyone wanting to call themselves a "software engineer", absolutely needs to
read "Engineering a Safer World" by Nancy G. Leveson.  Our industry lacks the
dicipline and rigour to have the privilege of calling ourselves "engineers".

Software is just the design of a machine to solve a task.  We like to think of ourselves as implementors, but that's the compiler not you.  You're the designer, yet we don't think about design enough.  We pick "the best tool for the job", which really isn't.

We're missing polymorphism at the system and service API levels.

Merging of data in versions of old schemas into new ones (e.g. to handle renames).

Special/reserved keywords in programming languages is a bad idea.

Maximise caching effectiveness.

Using the JVM with Docker is a terrible idea.  Redeploying the whole service looses all the optimisations performed by the JIT and any caching.


Many focus on dynamic vs. static typing, but really that is an insignificant part of it; an implementation detail.  Really the bigger picture is the dynamism of the language.  How much is is able to change over time, and while running.  Static is frozen in time.  Static vs. dynamic is a spectrum.

By sacrificing dynamism you can get more compile time checks.  But restricting how much the program can change over time, you can intelligently detect some small classes of bugs.  Statically typed languages achieve this by restricting possible types of values associated with variables, parameters and return values.

What if you embrace dynamism?  The value of static typing is at compile time, but if you have no compile time, what then?  If you're interacting with the language and program at runtime during development and in production, you have all that information and more.  Your server code runs more than you edit it, optimise for that.

Lisp and Smalltalk-style images aren't required when you don't plan on shutting down the server, but something similar could still be possible (e.g. restoring cache values).

IMO there is no reason that a truly dynamic language cannot be safe or reliable.  Just most work goes into static type checking.

Is the null reference really a billion $ mistake?

Comments as first class objects.  Other types of comments, e.g. tests, examples, schemas.

Self-healing, run forever, adapt over time.

Wasted compute running the JVM on tiny machines in Docker.

Catalyst will be built in a combination of itself, Clojure and Java.  Various libs will be used by the language environment and compiler.  Custom libs too, e.g. the messaging protocol.

If you're introducing gRPC, you should be asking yourself: "why is this a distributed system?  why am I trying to pretend that it isn't one?"

To choose not to learn a dynamic language because "I like types", is wilful ignorance and delusion.  You see it all the time when Lisp or Clojure are mentioned.  There's always a few who will outright reject any dynamic languages due to being dynamic.  I frequently see pathetic comments such as "anyone who designs a new dynamic language is an idiot, static typing is a requirement".  Not every programming language needs to be the same, nor should they be.  Type system is the wrong question.  Flexibility and system dynamism.  Fix issues at runtime.  Making sure the system is safe in the presence of software errors.

What's the priority?  (Safety.)  And once resolved, is fixing reliability even necessary?

There is no silver bullet.

---

> [...] It turns out this is very simple because of the simple generic
> extension.  Now, I'm not trying to claim that solves any important problem.
> Very careful about that.  This is a little, tiny feeling of what it is that's
> needed to make things more flexible.  Consider the value here.  The value is
> that I'm dynamically allowed to change the meaning of all my operators to add
> new things that they can do.  Dynamically.  Not at compile time, at runtime.
> Programs can produce things that are the new way to add.  [...] They can
> attach that to addition and they can take it off, and all that.  [...] That's
> wonderful.  What is it doing?  It's giving me tremendous flexibility.
> Flexibility because the program I wrote to run on a bunch of numbers, with
> only a little bit of tweaking, suddenly runs on matrices so long as I didn't
> make the mistake of commuting something wrong.  If I did I'm in trouble.
> This makes proving the theorems about things very hard.  In fact maybe
> impossible.  That's the cost, so the costs and the benefits are very extreme.
> I can pay correctness (proof of correctness, or belief in correctness) [...]
> for tremendous flexibility, right there.  Again, we worry
> about ideas like "is correctness the essential thing I want?".  Look, I'm not
> opposed to proofs, I love proofs, I'm an old mathematician.  The problem is
> that proofs—  Putting us into the situation that Dijkstra got us into about
> 30 (or 40 years) ago, where you're supposed to prove everything to be right
> before you write the program.  Getting yourself into that situation puts us
> into a world where we have to make the specifications for the parts as tight
> as possible.  Because it's hard to prove general things, except occasionally
> it's sometimes easier, but usually it is harder to prove a general theorem
> about something.  So you make a very specialised thing that works for
> a particular case.  You build this very big tower of stuff, and boy is that
> brittle!  Change the problem a little and it all falls over.  We didn't learn
> something from the fact that the electrical engineering I did, the digital
> abstraction whereby the inputs to something accept a much bigger range than
> the outputs are allowed to produce.  And therefore you get rid of noise at
> every stage.  That's the kind of flexibility that I hope to get.  When you do
> this kind of generic thing: extensions, it doesn't mean that old stuff
> breaks, it means the new stuff is not proven.
>
> — Gerald J. Sussman, 2011, [We Really Don't Know How to Compute](https://www.youtube.com/watch?v=HB5TrK7A4pI)



> Static typing is a trap.  Semantics about entities and their relations should
> not be static, but dynamic.  That's where flexibility and adaptability come
> from.  Too many devs throw that away due to fear.  "We never let you run
> a certain kind of mistake" feels safe, but in reality, "we can correct any kind
> of mistake while the program runs" is safer and unlocks many possibilities.
>
> — YouTube commenter.



Powerful hot reload that is the only option rather than being an optional
add-on or non existent.


Languages striving for maximum raw performance are typically designed for AOT
compilation; they tend to be highly static and often shift more work to the
programmer (e.g. manual memory management and defining types) to assist the
compiler in generating optimised code.

