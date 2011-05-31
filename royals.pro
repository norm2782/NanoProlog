ma(mien,juul).
ma(juul,bea).
ma(bea,alex).
ma(bea,cons).
oma(X,Z):-ma(X,Y),ouder(Y,Z).

append(nil,X,X).
append(cons(A,X), Y, cons(A,Z)):- append(X,Y,Z) .

pa(alex,ale).
pa(alex,ama).
pa(alex,ari).
ma(max,ale).
ma(max,ama).
ma(max,ari).
ma(bea,con).
ma(bea,fri).

elem(X, cons(X,Y)).
elem(X, cons(Z,Y)) :- elem (X, Y).
man(X) :- elem(X, cons(claus, cons(alex, cons(con, cons(fri, empty))))).

ouder(X,Y) :- pa(X,Y).
ouder(X,Y) :- ma(X,Y).

voor(X,Y) :- ouder(X,Y).
voor(X,Y) :- ouder(X,Z), voor(Z,Y).

plus(zero,X,X).
plus(succ(X), Y, succ(Z)) :- plus(X, Y,Z).
