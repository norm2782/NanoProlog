wellTyped(map,(A->B)->([A]->[B])).
wellTyped(foldr,(A->B->B)->B->[A]->B).
wellTyped(ap(F,V),A):-wellTyped(F, B->A), wellTyped(V,B).

