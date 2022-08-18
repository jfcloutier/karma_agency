:- module(genealogist, 
	[  ancestor_descendant/2,
	   siblings/2,
	   parent_child/2,
	   father_child/2,
	   mother_child/2,
	   assert_father_child/2,
	   assert_mother_child/2,	   
	   retract_father_child/2,
	   retract_mother_child/2
	]).


ancestor_descendant(X, Y) :- parent_child(X, Y).
ancestor_descendant(X, Z) :- parent_child(X, Y), ancestor_descendant(Y, Z).

siblings(X, Y) :- parent_child(Z, X), parent_child(Z, Y), X @< Y.

parent_child(X, Y) :- mother_child(X, Y).
parent_child(X, Y) :- father_child(X, Y).

parent_child(portia, liz).
parent_child(richard, liz).
parent_child(mado, jf).
parent_child(jf, lysandre).



:- dynamic mother_child/2, father_child/2.

assert_mother_child(Mother, Child) :-
	assert(mother_child(Mother, Child)).

assert_father_child(Father, Child) :-
	assert(father_child(Father, Child)).
	
retract_mother_child(Mother, Child) :-
	retractall(mother_child(Mother, Child)).

retract_father_child(Father, Child) :-
	retractall(father_child(Father, Child)).

