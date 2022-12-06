# Implementing Relational Language

In its essence the implementation of a simple relational/logic programming language really consists of only a few basic concepts.

Our implementation will not use techniques from the real-world. This very much means that the *real* implementation of such a language, one that could be used for solving real tasks, should not be implemented with such a design.

This write up describes a simple *abstract machine*. One that can be implemented on less than 300 lines of code in any practical language.
While the main focus of the design is not efficiency and near-native speed, our implementation will be able to compute factorial of 7 in just a little above 20 seconds. That is, of course, with our own natural number representation.

For more information on real-world implementation of logic programming langauges see [Warren Abstract Machine](https://en.wikipedia.org/wiki/Warren_Abstract_Machine).

> This write up expects the reader to have been exposed to a language like Prolog. No advance knowledge of Prolog or logic programming is required - only a level that can be obtained in a couple hours on the internet.

> We also expect that the reader understands [Peano Arithmetics](https://wiki.haskell.org/Peano_numbers) well enough to follow our examples containing addition and multiplication.

----

Let us start with a simple example of a Minilog program:
```prolog
plus(z, N, N).
plus(s(N), M, s(R)) :- plus(N, M, R).
```
Now suppose that we want to prove a query `plus(A, B, B)`.

> This reads: "What are two natural numbers whose sum is equal to the second one?"

Were we do that manually on a piece of paper with a pen, it might look something like this:

----
1) We start by writing down our *goal*:
```prolog
  plus(A, B, B)
```

2) Now we start looking for a predicate in the base that has the same name and arity as our goal. The first one fits that description so we try to unify our goal with the head of that predicate. We replace our current goal with a new one:
```prolog
  plus(A, B, B) = plus(z, N, N)
```

3) This can be done through *decomposition*. We break down the current goal into three smaller pieces:
```prolog
  A = z
  B = N
  B = N
```

4) We now have three *unification* goals. Let us start with the one at the top and observe that it is technically an *assignment*. We record this new information on the side of the paper (we just write it down) and discharge the *sub-goal*:

    Assignments: `A = z`.

```prolog
  B = N
  B = N
```

5) We now have two identical sub-goals. We will safe us some work and discharge both of them at the same time. The same as above, we just write it down:

    Assignments: `A = z`, `B = N`.

6) With that, we have got rid of all the goals. This means that our initial goal `plus(A, B, B)` can be satisfied when `A` is `z` and `B` is anything at all. (Because neither `B` nor `N` are assigned a concrete value.)

----

We are not done yet, however! One of the key features of logic langauges is the backtracking. That means we need to go back a little and see if we can satisfy the same goal in a different way. We need to do this for all potential paths through the *proof space*.

We go back to the step `2` and take an alternative path this time around.

2) We keep looking for a predicate in the base that has the same name and arity as our goal. The second one fits that description too, so we try to unify our goal with the head of that predicate. On top of that, we also take the goals in the *rule*'s body and write them down too:
```prolog
  plus(A, B, B) = plus(s(N), M, s(R))
  plus(N, M, R)
```

3) Once again, we decompose the unification goal at the top:
```prolog
  A = s(N)
  B = M
  B = s(R)
  plus(N, M, R)
```

4) The next sub-goal goes stright into the "environment" part of our papers/text files:

    Assignments: `A = s(N)`.

```prolog
  B = M
  B = s(R)
  plus(N, M, R)
```
5) The two following goals are clearly related to each other. Both look like a simple assignment on their own, but both are assigning to `B`. We do not mind that in the slightest. The assignment will just look a little bit more complicated:

    Assignments: `A = s(N)`, `B = M = s(R)`.

```prolog
  plus(N, M, R)
```

6) For our current goal we, once again, turn our attention to the base and search for a fitting predicate. We try the first one again. This time around, however, we need to do some "bookkeeping" so that our names do not get tangled:

    Assignments: `A = s(N)`, `B = M = s(R)`.

```prolog
  plus(N, M, R) = plus(z, N1, N1)
```

7) We decompose again:

    Assignments: `A = s(N)`, `B = M = s(R)`.

```prolog
  N = z
  M = N1
  R = N1
```

8) We extend the assignments:

    Assignments: `A = s(N)`, `B = M = s(R)`, `N = z`.

```prolog
  M = N1
  R = N1
```

9) We do it again:

    Assignments: `A = s(N)`, `B = M = N1 = s(R)`, `N = z`.

```prolog
  R = N1
```

10) And now again:

    Assignments: `A = s(N)`, `B = M = N1 = R = s(R)`, `N = z`.

----

The keen eyed reader should be able to observe that we have introduced a sort of a cycle between `R` and its assigned "value". Indeed, `R` is assigned a term of the shape `s(R)`. This very much means that `R` is defined in terms of itself.

In Prolog, this is allowed by default. Not so much in Minilog. Any attempt to introduce this sort of a cycle results in a failure of the process.

So we need to backtrack again. This time we go back to the step `6` and take yet another path:

6) We now try the second one again. We need to do some "bookkeeping" as well:

    Assignments: `A = s(N)`, `B = M = s(R)`.

```prolog
  plus(N, M, R) = plus(s(N1), M1, s(R1))
  plus(N1, M1, R1)
```

7) We decompose again:

    Assignments: `A = s(N)`, `B = M = s(R)`.

```prolog
  N = s(N1)
  M = M1
  R = s(R1)
  plus(N1, M1, R1)
```

8) We can fast forward through the next three sub-goals:

    Assignments: `A = s(N)`, `B = M = M1 = s(R)`, `N = s(N1)`, `R = s(R1)`.

```prolog
  plus(N1, M1, R1)
```

----

We could go on and keep evaluating, but that would not lead to anything useful. From now on, the process goes on for ever.
The last goal above is equivalent to `plus(N1, s(s(R1)), R1)`.

We could read it as: "Which two natural numbers (where the second one is not smaller than `2`), when added together add up to a difference between the second one and number `3`.

The answer is `None!` - there is no way to add two natural numbers together and end up with a result smaller than one of those numbers.

This is not a fault of Prolog/Minilog, our definition of `plus` is simply not compatible with all the different *modes* in which we can invoke it in logic programming environment.

----

We now turn our attention to the strategy that we have used in the process above.
The idea was to carry around an environment recording assignments of variables.

This strategy is not inherently bad, it would work not only on paper but also as an underlying design for our *abstract machine*. It would have a couple of strong drawbacks, however. It would not be very efficient and the implementation would be a bit more involved than we would expect from a **simple** *abstract machine*.

As a bonus, this approach get quite unwieldy quite quick.

For those reasons we leave this strategy behind and focus on a much more tractable one.

Instead of recording those assignments in an environment, we apply them immediatelly like they are some sort of substitutition. To be more specific, any time we have a sequence of goals and the top one is in the shape `<variable> = <a term>`, we treat it as a substitution and apply it to the rest of the goals.

This approach is part of the algorithm for unification introduced by [Martelli and Montanari](https://dl.acm.org/doi/10.1145/357162.357169).
It is also nicely summed up in this [wikipedia acrticle](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm).

We will get back to it in later sections.


----
## Representation of Terms

This section describes our way of representing terms and other syntactic forms of our langauge as simple Haskell data structures.

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

We split the whole issue at hand into two parts.

- unification
- proof search with backtracking

Those two concepts cover the entirety of the evaluation.
But we have hinted that with our leading example already.

### Unification

In its core, the concept of unification is a really simple one.

We have two things and we ask whether they can be "the same thing" under some specific circumstances.

Here is an example (the operator `=` means `unify with`):
```prolog
  ?- A = something .
```

The result of running such a query in Prolog will, of course, be positive. Simply because a variable like `A`, that is - a fresh one - can be unified with anything at all.

So let us first give a few vague rules for unification before we explore any further:

- An atom unifies with an identical atom.
- A struct unifies with a struct when they have the same name and the same arity and their arguments unify pairwise.
- A fresh variable unifies with anything.
- A variable that has already been unified with something unifies with another thing only if the first thing and the new thing unify together.

This should serve as a mental checkpoint before we go all in on the real algorithm mentioned above.

---


### Proof Search with Backtracking

In this section we are going to explore the concepts of proof search and backtracking very briefly. We will not go into a much detail simply because the key element of our approach will rely on the specific representation of a state of our `machine`.

#### Proof Search

The traversal of the state space is quite stright forward. We start with an initial goal and we search for a predicate in our knowledge base that would allow us to prove that goal.

When we find such a predicate we have to see if our goal can unify with the head of the predicate. If it does and the predicate is a *fact*, we have found a way to satisfy the goal.
If it is *rule* we have to attempt proving the body of the rule. Remember - bodies of rules are just goals.

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

This is quite important, because it allows us to use unification in this part of the process too. Later it will be aparent just how important and central unification is to the whole process of evaluation - **it is** the thing that does most parts of the evaluation.

----

#### Backtracking

We have already mentioned backtracking in the previous section. In this section we will discuss it a little bit more.

We can observe that in our small language backtracking should really only happen when we have a predicate invokation and we are searching for a fitting predicate in the base. We do backtrack by trying **all the fitting** predicates in the base for the current goal.

There is no more to it than that. So if we are able to come back to a point where we have decided to try the first fitting predicate in the base and try to use another one, we would have a backtracking handled.

The only viable way to "come back to a past point" in our implementation is to store our machine state (or some relevants parts of it) somewhere until the current path is done being explored. When it eventually succeeds or fails we can "get back" to the stored one - doing backtracking.

So it all depends on the representation that we chose for our machine state. That is precisely the topic of the next section.


### Machine State Representation

We want a representation that is explicit enough - does not leave any part of the evaluation to be "implicitly" encoded in the implementation langauge - but is simple enough at the same time. We do not want to use complex data structures making the reasoning about or the re-implementation of Minilog more complicated than it needs to be.

Fortunately, we will do with just a few basic data types that should be common enough not only to most programming languages but to any of the readers too.

What our machine state needs to contain:

- our base
- a position in the base
- a stack of goals to satisfy
- a stack for backtracking

For the bookkeeping purposes (renaming variables to fresh names) we will also need to carry some increment-only counter.

And to make the presentation of the success easier we also keep around a mapping from all the variables from the original query to the terms that the evaluation assigned to them.
Thanks to that, the presentatin is just a matter of printing all the mappings.

----

All of this leads to the following Haskell data structure:
```haskell
data State
  = State { base                :: [Predicate]

          , position            :: Int
          , goal'stack          :: [Goal]
          , backtracking'stack  :: [([Goal], Int, Query'Mapping)]

          , counter             :: Int
          , query'vars          :: Query'Mapping }

type Query'Mapping = Map.Map String Term
```

----


### Algorithm

((TODO: Write the full definition of the unification algo.))

#### Unification

The actual algorithm we will implement consists of the following rules:

- unification of two identical terms is trivial
- unification of two structs of the same name and the same arity leads to *decomposition* (+ the occurs check)
- unification of two structs that do not satisfy the requirements above fails
- unification of a variable and a term is used as a substitution on the rest of the goals (+ the occurs check)


> The occurs check is a mechanics preventing unifite terms. It fails whenever we would try to unify a variable with a term featuring that variable.


#### Step of the Evaluation

With the unification as described above we can go on and describe the algorithm that drives the evaluation.

A single step of the evaluation:

- checks if there is a goal on top of the *goal stack*.
  - If the goal is to **invoke** a predicate we search for a first potentially fitting predicate in the base. (One with the same name and the same arity.)
    - If we find one and it is a *fact*, we replace the previous goal with an *unification* goal to unify the original goal (now shaped into a *struct*) with the head of that *fact* (which also is shaped into a *struct*).

    - If we find one and it is a *rule*, we do the same as above **and** on top of that we put the *body* of the rule on the *goal stack* too. The order is - the unification on top then the body then whatever was already there.

    - If we have not found one, we fail the current *goal stack*.

  - If we have succeeded in searching for fitting predicate in the previous step, we also back-up our original *goal stack*, *position* and *free var query mapping* on top of the *backtracking stack* so that we can come back to it later.

  - If the goal on top of the *goal stack* is to **unify** two things, we use the unification algorithm from the previous section. THe unification may apply substitution to our current *goal stack* and the *free var query mapping*.
    - If the unification succeeds we obtain a new *goal stack* and a new *free var query mapping*.
    If the unification fails, we fail the current *goal stack*.

  - If there is no goal, this means that the previous step either failed or succeeded. We need to attempt backtracking. We check if there is something on the *backtracking stack*:
    - If there is, we reset our state according that backtracking record and pop it from the *backtracking stack*.
    - If there is not, we are done. The evaluation concluded.

> Note about the renaming: We have omited the mention of the renaming. We suspect that the reader has been able to figure it out already but for the sake of the completeness - Any time we do sort of an instantiation of a predicate from the base, we rename all of the variables in it to completely new and unique names.


