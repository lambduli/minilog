# minilog

## TODO:
- [ ] refactor State + Action for more type safety
- [ ] implement serialization of the assignments
- [ ] finish REPL
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
