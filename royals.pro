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

ouder(X,Y) :- pa(X,Y).
ouder(X,Y) :- ma(X,Y).

voor(X,Y) :- ouder(X,Y).
voor(X,Y) :- ouder(X,Z), voor(Z,Y).

plus(zero,X,X).
plus(succ(X), Y, succ(Z)) :- plus(X, Y,Z).
