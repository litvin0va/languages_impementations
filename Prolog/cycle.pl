seq(From,_,From).
seq(From,To,X) :-
    From<To,
    Next is From+1,
    seq(Next,To,X).

loop(N) :-
    seq(1,N,Index),
    do_something(Index),
    fail.
loop.

do_something(Index) :-
  display(Index), nl.

:- initialization(main).
main :-
  write('Input N: '),nl,
  read_integer(N), nl,
  loop(N), halt.
