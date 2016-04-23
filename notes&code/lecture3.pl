factorial(0,1).
factorial(N,X) :- M is N - 1, factorial(M,Y), X is N * Y.

% A version of factorial that lets us put a variable in
factorial2(0,1).
factorial2(N, X) :- factorial2(M, Y), N is M + 1, X = Y * N.

% A "cut" is the proposition !. ! is always true, but prolog will not
% backtrack over the cut. Any variable that has a value when we cross
% the cut will keep the value
