plus(z, N, N).
plus(s(N), M, s(R)) :- plus(N, M, R).
times(z, _, z).
times(s(N), M, A) :- times(N, M, R), plus(R, M, A).
fact(z, s(z)).
fact(s(N), R) :- fact(N, PR), times(s(N), PR, R).
