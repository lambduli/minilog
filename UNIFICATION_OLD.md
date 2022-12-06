### Unification

In its core, the concept of unification is a really simple one.

We have two things and we ask whether they can be "the same thing" under some specific circumstances.

Here is an example (the operator `=` means `unify with`):
```prolog
  ?- A = something .
```

The result of running such an expression in Prolog will, of course, be positive. Simply because a variable like `A`, that is - a fresh one - can be unified with anything at all.

So let us give all the rules (in a vague way) for unification before we explore any further:

- An atom unifies with an identical atom.
- A struct unifies with a struct when they have the same name and the same arity and their arguments unify pairwise.
- A fresh variable unifies with anything.
- A variable that has already been unified with something unifies with another thing only if the first thing and the new thing unify together.

> Further down this write up we will discuss the unification in more detail and rigor. Ee will even reference and use formally correct algorithm for it. For now, however, the vague definition above will do.

Just from this definition we could already come up with a way to implement this notion of unification.

The parts about concrete values (atoms and structs) would be simple. Structs would then decompose to unification on pairs of arguments.
The part that is in any way interesting is how will we deal with variables.

We could have a mapping between variable names and their "states".
Such a state would reflect four stages that a variable can happen to be in during the program evaluation.

A variable can be either:

- `fresh`, or
- unified with another fresh variable, we could say it has been `fused` together with the other one, or
- unified with a value (a struct or an atom), we could say it has been `assigned` that value, or
- unified with some fresh variable and also some value (in no particular order), then we could say it is `fused and assigned` at the same time.

> This notion does not really follow the way that unification would be implemented, but it can be nice for visualization purposes. Later we will be able to see why it might be nicer than the more traditional approach.

Now whenever we are asked to unify two things - for example a varible `X` and a struct `foo(a, b)`, we can look into our environment and see what state that variable is in.
Suppose that `X` would be `fused` with another variable, we would then simply "promote" those variables into being `fused and assigned` a value `foo(a, b)`.

----
In case you know quite something about unification, you might be wondering why would we take this approach.

The reasoning is very simple - this might be the way you would chose to solve unification equations on a paper. You would go one equation at a time and each time you would "fuse" two variable together or "assign" some variable a value, you would write this newly found piece of information on the side of the paper - building a sort of environment in the process.

----


((TODO: I should try to really go into detail about the notion of unification I have established above. If it works - great, if it does not, I can delete it and go directly for the Martelli's algorithm explaining how we apply substitutions to the stack of goals directly.))
