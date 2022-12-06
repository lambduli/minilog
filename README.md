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


## How can you use it?

1) You run `cabal build` and if that succeeded `cabal run`.

2) You can use the knowledge base in the repository, modify it, or use your own. It is loaded into the repl using a command `:load <path to the file>` like `:load factorial.pl`.

3) You write Prolog-like query and hit enter.

4) When presented with a result, you either write `:next` and hit enter (to backtrack) or you write `:done` and hit enter to conclude the computation.

5) When you want to quit the REPL, you submit `:q` or `:Q`.
