% append two lists
% myappend (list1, list2, resultlist) is true if and only if resultlist
% is the result of appending list1 and list2

myappend([],L,i).
myappend([H | T], L ,[H | R]) :- myappend(T, L, R).

% (define myappend
%    (lambda (l1 l2)
%        (if (null? l1)
%            l2
%            (cons (car l1) (myappend (cdr l1) l2))))

contains(X, [X | T]).
contains(X, [H | T]) :- contains(X, T).

% insertbefore a x [w x y z] => [w a x y z]

insertbefore (A, X, [X|_],[A,X|_]).
insertbefore (A,X,[H|T1],[H|T2]) :- insertbefore(A,X,T1,T2).

% insertbeforeall a x [w.x.y.x.z.x] => [w,a,x,y,a,x,z,a,x]

insertbefore (_, _, [],[]).
insertbefore (A, X, [X|T1],[A,X|T2]) :- insertbefore(A,X,T1,T2).
insertbefore (A,X,[H|T1],[H|T2]) :- insertbefore(A,XT1,T2).

% flatten

% factorial
factorial(0,1).
factorial(N,X) :- M is N - 1, factorial(M,Y), X is Y * N.
