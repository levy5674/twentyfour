% Play the game of twenty four
% given four numbers, combine them
% with addition, subtraction, multiplication, division
% to get a final value of 24.
%
% Output of win is prefix notation for solution

op('+', A, B, R) :- R is A + B,
  A >= B.
op('-', A, B, R) :- R is A - B.
op('x', A, B, R) :- R is A * B,
  A >= B.
op('/', A, B, R) :- R is A / B.

solve(A, B, C, D, VAL, [OP3, OP2, OP1, W, X, Y, Z]) :-
  ensure_loaded(library(lists)),
  permutation([A,B,C,D], [W,X,Y,Z]),
  op(OP1, W, X, R1),
  op(OP2, R1, Y, R2),
  op(OP3, R2, Z, VAL).

solve(A, B, C, D, VAL, [OP3, OP2, W, X, OP1, Y, Z]) :-
  ensure_loaded(library(lists)),
  permutation([A,B,C,D], [W,X,Y,Z]),
  op(OP1, Y, Z, R1),
  op(OP2, W, X, R2),
  op(OP3, R2, R1, VAL).

solve(A, B, C, D, VAL, [OP3, W, OP2, X, OP1, Y, Z]) :-
  ensure_loaded(library(lists)),
  permutation([A,B,C,D], [W,X,Y,Z]),
  op(OP1, Y, Z, R1),
  op(OP2, X, R1, R2),
  op(OP3, W, R2, VAL).

win(A,B,C,D,EXPR) :-
  solve(A,B,C,D,VAL,EXPR),
  nth(_, [24, 24.0], VAL).


% special case for 8 / (1 - (2/3))
win(A, B, C, D, ['/', Z, OP, Y, '/', W, X]) :-
    ensure_loaded(library(lists)),
    permutation([A,B,C,D], [W,X,Y,Z]),
    nth(_, ['+', '-'], OP),
    XY is X * Y,
    XZ is X * Z,
    op(OP, XY, W, R1),
    op('/', XZ, R1, 24.0).
