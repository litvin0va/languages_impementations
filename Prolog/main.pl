%
% The program for finding the root of the equation f (x) = 0
% in a given interval ab with an accuracy of eps by
% the method of quadratic interpolation.
% If successful, the function displays the root and
% the number of iterations spent looking for it.
% Otherwise the error code.
%
% Compile : gplc main.pl
% Run: ./main
%

f(X,Y) :- Y is X * X - 4.

inter(X1, X2, X3, Y1, Y2, Y3, RES, RET) :-
  Y11 is Y2 * X1 - Y1 * X2,
  DIF12 is X1 - X2,
  E is 0.00000000001,
  (abs(DIF12) < E ->
  RES is 0,
  RET is 0
  ;
  Y12 is Y11 / DIF12,
  Y21 is Y3 * X2 - Y2 * X3,
  DIF23 is X2 - X3,
  (abs(DIF23) < E ->
  RES is 0,
  RET is 0
  ;
  Y22 is Y21 / DIF23,
  Y13 is Y22 * X1 - Y12 * X3,
  DIF13 is X1 - X3,
  (abs(DIF13) < E ->
  RES is 0,
  RET is 0
  ;
  RES is Y13 / DIF13,
  RET is 1
  )
  )
  ).

loop(IT, X1, X2, X3, Y1, Y2, Y3, EPS) :-
  CUR_IT is IT + 1,
  (CUR_IT > 1234 ->
  write('MAXIT error'), nl,
  halt
  ;
  inter(Y1, Y2, Y3, X1, X2, X3, RES, RET),
  XN is RES,
  f(XN, YN),
  (RET = 0 ->
  write('Res: -1'), halt
  ;
  (abs(YN) < EPS ->
  write('Ans:'),
  write(RES), nl,
  write('It:'),
  write(CUR_IT), nl,
  halt
  ;
  index_to_set(Y1, Y2, Y3, YN, ID),
  (ID = 0 ->
  loop(CUR_IT + 1, X1, X2, X3, Y1, Y2, Y3, EPS)
  ;
  (ID = 1 ->
  loop(CUR_IT + 1, XN, X2, X3, YN, Y2, Y3, EPS)
  ;
  (ID = 2 ->
  loop(CUR_IT + 1, X1, XN, X3, Y1, YN, Y3, EPS)
  ;
  (ID = 3 ->
  loop(CUR_IT + 1, X1, X2, XN, Y1, Y2, YN, EPS)
  ;
  fail
  )
  )
  )
  )
  )
  )
  ).

compare_abs(A, B) :- abs(A) >= abs(B).

index_to_set(Y1, Y2, Y3, YN, ID) :-
  (compare_abs(YN, Y1) ->
  (compare_abs(YN, Y2) ->
  (compare_abs(YN, Y3) ->
  write('Res: -2'), nl, halt
  ;
  ID is 3
  )
  ;
  (compare_abs(Y2, Y3) ->
  ID is 2
  ;
  ID is 3
  )
  )
  ;
  (compare_abs(Y1, Y2) ->
  (compare_abs(Y1, Y3) ->
  ID is 1
  ;
  ID is 3
  )
  ;
  (compare_abs(Y2, Y3) ->
  ID is 2
  ;
  ID is 3
  )
  )
  ).

solve(A, B, EPS, X) :-
  X1 is A,
  X2 is B,
  X3 is (A + B) / 2,
  f(X1, Y1),
  f(X2, Y2),
  f(X3, Y3),
  (abs(Y1) < EPS ->
  X is X1
  ;
  (abs(Y2) < EPS ->
  X is X2
  ;
  (abs(Y3) < EPS ->
  X is X3
  ;
  loop(0, X1, X2, X3, Y1, Y2, Y3, EPS),
  X is 42
  )
  )
  ).

:- initialization(main).
main :-
  write('Input a: '),
  read_number(A), nl,
  write('Input b: '),
  read_number(B), nl,
  write('Input eps: '),
  read_number(EPS), nl,
  solve(A, B, EPS, ANS),
  write('Ans: '),
  write(ANS), nl,
  halt.

