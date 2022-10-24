:- module(simple, [perfect/1, outb/1], [assertions, regtypes]).

% :- pred X(A) : a(A) => b(A).

:- regtype b/1.
b(b0).
b(b1).

% OK(perfect) post-query == post-pred
:- pred perfect(A) => b(A).
perfect(b0).
perfect(b1).

% OK(particular case) post-query \superset post-pred
reduced(b0).

% OK(particular case) post-query \superset post-pred
reduced2(b1).

% NO(disjoint) post-query \cap post-pred = empty
outb(z).

% NO(spurious) post-query \subset post-pred
mixed(z).
mixed(b0).
mixed(b1).

% NO(spurious)
mixed2(z).
mixed2(b1).
