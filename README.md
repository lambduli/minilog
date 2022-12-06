# Minilog

Minilog is a very small programming language implementation.
It's goal is to capture the essence of relational/logic programming.

> For the implementation write up see: [the write up](./WRITEUP.md).

It is very much a subset of Prolog, at least in terms of a syntax.
The biggest (intended) difference in terms of a runtime behaviour is the strict
`occurs` checking.
Unlike Prolog, Minilog does not accept infinite terms.

This means that the following snippet will succeedin Prolog but not in Minilog:

```prolog
X = foo(X).
```

## Syntax
As stated above, Minilog's syntax is a subset of the syntax of Prolog.
In Minilog you have:

- Atoms

  `foo`, `z`

- Variables

  `X`, `Y`
- Wildcards

  `_`
- Structs

  `foo(X, thing)`

- Facts

  `plus(z, N, N).`

- Rules

  `plus(s(N), M, s(R)) :- plus(N, M, R).`

- Conjunction

  `times(N, M, R), plus(R, M, A).`

- Unification Operator

  `X = something`

----

Here is an example of a valid Minilog knowledge base definition:
```prolog
  plus(z, N, N).
  plus(s(N), M, s(R)) :- plus(N, M, R).

  times(z, _, z).
  times(s(N), M, A) :- times(N, M, R), plus(R, M, A).

  fact(z, s(z)).
  fact(s(N), R) :- fact(N, PR), times(s(N), PR, R).
```

And here is an example of a valid query:
```prolog
?- fact(A, B) , plus(A,B, s(s(z))) .
```

---

## What is not in Minilog?

- numbers
- arithmetics (obviously)
- strings
- explicit or (`;`)
- cuts (`!`)

## What is it for?

The implementation is just a complement of the [write up](./WRITEUP.md).
The goal of this project is to offer an introductory level almost tutorial-like description of an implementation design for such a language.

The design of the implementation is not aming to represent a practical implementation of a relational programming language.
It should not be more than an initial insight into the ideas behind concepts like
unification, proof search, that such a relational language does and backtracking
in such a proof search.

You could also say that the design of the implementation was chosen to be
ver observable by default. Indeed, as the runtime is implemented
as a simple "stepping" interpreter, we can very much see "inside" the process.
The write up goes into more detail on that topic.

The goal of the project is not to be a rigorous introduction into the matter! At best it should serve as a toy-like demonstration. Similar to when a math or physics teacher reaches for a vast (but still valid) simplification to make some complicated sounding concept more approachable.


## TODO:
- [ ] refactor State + Action for more type safety
- [x] implement serialization of the assignments
- [x] (somewhat) finish REPL
- [ ] experiment with strictness annotations


fact 7

Using my "lazy" approach:

- 2:01.30
- 2:01.41
- 2:00.52
- 2:02.62
- 2:00.52

Using Martelli's unification algo:

- 28.762
- 26.021
- 27.814
- 26.337
- 28.376






##### New unification.
Each goal is processed.
Calls are processed the same way - they decompose.
But Unify goals are processed differently:

  - t = t               DELETE
  - struct = struct'    DECOMPOSE
  - struct = struct'    CONFLICT if names or arities are not the same

  - NON'VAR = VAR       SWAP (maybe not necessary)
  - VAR = ANYTHING?     ELIMINATE (and I apply that substitution to all the goals on the goal'stack)
    if the thing on the right is a variable, I think it should be OK too
    can there be a cycle though?
    [ A = C
    , A = B
    , B = C
    , C = A ] <- TOP
    ELIMINATE the `C = A`
    [ A = A
    , A = B
    , B = A ] <- TOP
    ELIMINATE `B = A`
    [ A = A
    , A = A ] <- TOP
    DELETE `A = A` (twice)
    []
    OK so from that example I intuitively feel like I can't cause a cycle.
  
  - VAR = NON'VAR       OCCURS CHECK (I need to do that for every applicable Goal as a first thing!)




So the `unify` function takes two Values and a goal'stack + a Map.Map String Value (more about that later, this is for query variables).

It unifies those two values and depending on the case it might apply the substitution to all the sub-goals on the goal'stack.
It also applies that substitution to the Map.Map String Value.
Then it returns `Maybe ([Goal], Map.Map String Value)`.
