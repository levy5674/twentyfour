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

isdigit(X) :- ensure_loaded(library(lists)),
        nth(_, [1,2,3,4,5,6,7,8,9], X).

isop(X) :- ensure_loaded(library(lists)),
        nth(_, ['+', '-', 'x', '/'], X).

winner(24).
winner(24.0).

solve([[V1,X1], [V2,X2]], [VAL, [OP, X1, X2]]) :- op(OP, V1, V2, VAL).
solve([[V1,X1], [V2,X2] | T], ANS) :- op(OP, V1, V2, VAL1),
        solve([[VAL1, [OP, X1, X2]] | T], ANS).
solve([[V1,X1], [V2,X2] | T], ANS) :- op(OP, V1, V2, VAL1),
        solve([T | [VAL1, [OP, X1, X2]]], ANS).



win(A,B,C,D,S) :- ensure_loaded(library(lists)),
        permutation([A,B,C,D], [W,X,Y,Z]),
        solve([[W,W], [X,X], [Y,Y], [Z,Z]], [VAL, S]),
        winner(VAL).

win(A,B,C,D,S) :- ensure_loaded(library(lists)),
        permutation([A,B,C,D], [W,X,Y,Z]),
        solve([[W,W], [X,X]], ANS1),
        solve([[Y,Y], [Z,Z]], ANS2),
        solve([ANS1, ANS2], [VAL,S]),
        winner(VAL).

% special case [/, 6, [-, 1, [/, 3, 4]]]
win(A,B,C,D,['/',Z, [OP, Y, [/, X, W]]]) :- ensure_loaded(library(lists)),
        permutation([A,B,C,D], [W,X,Y,Z]),
        nth(_, ['+', '-'], OP),
        WY is W * Y,
        WZ is W * Z,
        op(OP, WY, X, R1),
        op('/', WZ, R1, 24.0).
