# Dynamism

___Last updated: 2024-10-27___

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


