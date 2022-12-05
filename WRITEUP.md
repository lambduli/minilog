# Implementing Relational Language

In its essence the implementation of a simple relational/logic programming language really consists of only a few basic concepts.

Our implementation will not use techniques from the real-world. This very much means that the *real* implementation of such a language, one that could be reasonably used for solving real tasks, would not be implemented with such a design.

For more information on real-world implementation of logic programming langauges see [Warren Abstract Machine](https://en.wikipedia.org/wiki/Warren_Abstract_Machine).

> This write up expects the reader to have been exposed to a language like Prolog. No advance knowledge of Prolog or logic programming is required - only a level that can be obtained in a couple hours on the internet.

----

## Representation of Terms

We start with the representation of Minilog programs.

### Basic Terms

#### Atoms

Representing *atoms* is going to be trivial. The only thing that needs to be store is the atom's name.

#### Variables

The situation is very similar when it comes to *variables*. Once again, we only need to keep around the name of the variable.

#### Compound Terms/Structs

In Minilog, as well as in Prolog, we have a notion of compound terms. People sometimes call them structs. They consist of a functor (a name of the sort of a "constructor") and a parenthesized list of terms delimited with a comma between them.

The `foo(something, B)` is a compound term/struct. The `foo` is the functor and `something` and `B` are the terms "stored" in the "structure".

#### Wildcard `_`

The simplest of all the terms is a wildcard/hole. It behaves similarly to a variable. The only exception is that it can never be referenced again, it serves only as a sink hole for anything. Despite that, or rather precisely for that, it can be quite usefull!

----

We could represent our notion of a *term* as a following Haskell data structure:
```haskell
data Term = Var String
          | Atom String
          | Compound Struct
          | Wildcard

data Struct = Struct{ name :: String, args :: [Term] }
```

----

### Predicates

What we call *predicate* is a more complicated form in the language. It is a way to define new relations. Our knowledge bases will consist of list of predicates.

A *Predicate* is either a *Fact* or a *Rule*.

#### Facts

A fact is syntactically very simple. It is predicate that does not have a body.
For example, `id(I, I).` is a fact.

We can observe that a fact is very similar to a compound term with the only difference being the fact that a fact is a predicate and as such it is always followed by a period.

In any case, we can take advantage of that fact when representing predicates in our implementation.

#### Rules

Rules are a little bit more complicated. They consist of two parts delimited by a symbol `:-`. The part on the left is basically a *fact* while the part on the right is called a *body*. The body of the rule is simply a non-empty list of *goals*.

We will cover what exactly the *goal* is later. For now, we just say that the body might be a *predicate call* or a conjunction.

### Goals

We have mentioned *goals* above. What we mean when we say that something is a goal is that it is a *term* that is understood as a proposition. Simply put, when we write the following query:
```prolog
id(something, A).
```
we say that `id(something, A)` is a goal. That is - we want it to be proved or disproved.

In this sense, bodies of rules are indeed just goals. For the rule to succeed all the sub-goals in the body need to succeed, assuming that the *head* of the rule was unified with the original goal anway.

Here is an example to further the point:
```prolog
... 

is_nice_color(X) :- is_color(X), is_nice(X) .
```
When we ask the query `is_nice_color(red)`, we can see how that query is considered a goal and how the body of our rule is considered a sequence of goals too.


----

We can represent predicates and goals as the following Haskell data structures:
```haskell
data Goal = Call Struct
          | Unify Term Term

data Predicate  = Fact Struct
                | Struct :- [Goal]
```
> Note about Goal: We will explain its two variants in greater detail in later sections about evaluation.

> Note about syntax: We have used an infix constructor `(:-)` to represent rules. 

----

## Evaluation - Basic Concepts

We split the whole issue at hand into a couple of concepts.

- unification
- proof search with backtracking

Of course, there is little bit more to even our small implementation.
We need to take care of parsing or at least representation of `terms` of the language. This will be covered in the next section called `Representation`.

For now we will keep discussing the core aspect of our small language - how it runs programs.

### Unification

In its core, the concept of unification is really a simple one.
We have two things and we ask whether they can be "the same thing" under some specific circumstances.

Here is an example (the operator `=` means `unify with`):
```prolog
  ?- A = something .
```

The result of running such an expression in Prolog will, of course, be positive. Simply because a variable like `A`, that is - a fresh one - can be unified with anything at all.

So let us give all the rules (in a wague way) for unification before we explore any further:

- An atom unifies with identical atom.
- A struct unifies with a struct when they have the same name and the same arity and their arguments unify pairwise.
- A fresh variable unifies with anything.
- A variable that has already been unified with something unifies with another thing only if the first thing and the new thing unify together.

> Further down this write up we will discuss the unification in more detail and rigor, we will even reference formally correct algorithm for it. For now, however, the vague definition above will do.

Just from this, arguably vague, definition we could already come up with a way to implement this notion of unification.

We could have a mapping between variable names and their "state".
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
In case you know quite something about unification, you might be wondering why would you take this approach.

The reasoning is very simple - this might be the way you would chose to solve unification equations on a paper. You would go one equation at a time and each time you would "fuse" two variable together or "assign" some variable a value, you would write this newly found piece of information on the side of the paper - building a sort of environment in the process.

----

### Proof Search with Backtracking

In this section we are going to explore the concept of backtracking very briefly. We will not go into a much detail simply because the key element of our approach will rely on the specific representation of a state of our `machine`.

#### Proof Search

The traversal of the state space is quite stright forward. We start with an initial goal and we search for a predicate in our knowledge base that would allow us to prove that goal.

When we find such a predicate we have to see if our goal can unify with the head of the predicate. If it does and the predicate is a fact, we have found a way to satisfy the goal.
If it is rule we have to attempt proving the body of the rule. Remember - bodies of rules are just goals.

Here is an example to help with making the point:
```prolog
...

small(mouse).

small(X) :- small(Y) , at_most_as_big_as(X, Y) .
```

If our initial goal is `small(mouse)` we can see that the first predicate in the base does very much match the goal at hand. But as we should already know, the key feature of logic languages is backtracking.
This means that even after the first predicate allows us to satisfy the goal, we still need to try to satisfy it in any other way that is possible.

The head of the second predicate in the base also matches our goal.
So if we can satisfy the goals that make up its body, we can also prove that `mouse` is `small`. The details of that process very much depend on the specific definition of `at_most_as_big_as` and rest of the definition for `small`. We will leave this example now.


----

The description above illustrates one important point - when our goal is a predicate invokation, we can approach satisfying it by viewing the invokation as a struct/compound term and see the heads of predicates the same way. Whenever we can unify those two terms (our goal and a predicate head) we have a potential way to satisfy the goal.

This is quite important, because it allows us to use unification in this part of the process too. Later it will be aparent just how important and central unification is to the whole process of evaluation - **it is** the thing that does the evaluation.

----

#### Backtracking

We have already mentioned backtracking in the previous section. In this section we will discuss it little bit more.

We can observe that in our small language backtracking should really only happen when we have a predicate invokation and we are searching for a fitting predicate in the base. We do backtrack by trying **all the fitting** predicates in the base for the current goal.

There is no more to it than that. So if we are able to come back to a point where we have decided to try the first fitting predicate in the base and try to use another one, we would have a backtracking handled.

The only viable way to "come back to a past point" in our implementation is to store our machine state somewhere until the current path is being explored and when that one succeeds or fails we can "get back" to the stored one - doing backtracking.

So it all depends on the representation that we chose for our machine state. That is precisely the topic of the next section.


### Machine State Representation

We want a representation that is explicit enough - does not leave any part of the evaluation to be "implicitly" encoded in the implementation langauge - but is simple enough at the same time. We do not want to use complex data structures making the reasoning about or the re-implementation of Minilog more complicated in other langauges.

Fortunately, we will do with just a few basic data types that should be common enough not only to most programming languages but to any of the readers too.




### Algorithm

