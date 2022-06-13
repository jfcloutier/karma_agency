:- module(tom, [analyze/1]).

:- use_module(library(chr)).

:- chr_constraint category_of/2, food_for/2.


% Only one category per named individual.
category_of(N, C1) \ category_of(N, C2) <=> C1 = C2.
% Only one category is food source for a category.
food_for(C, C1) \ food_for(C, C2) <=> C1 = C2.

sentences --> [] ; sentence, b_star, sentences.
sentence --> name(N), b, (`is`;`was`), b, (`a` ; `an`), b, name(C), stop, {category_of(N, C)}.
sentence --> name(N1), b, `eats`, b, name(N2), stop, {category_of(N1, C1), category_of(N2, C2), food_for(C2, C1)}.
stop --> `.`.
stop --> `,`.
b --> ` ` ; ` `, b.
b_star --> [] | b.
name(N) --> [Char], {letter(Char), string_codes(N, [Char])}.
name(N) --> [Char | More], {letters([Char | More]), string_codes(N, [Char | More])}.

letter(Char) :- Char > 64 , Char < 122.
letters([]).
letters([Char | Rest]) :- letter(Char), letters(Rest).


analyze(Input) :- phrase(sentences, Input).

% analyze(`Garfield eats Mickey. Tom eats Jerry, Jerry was a mouse. Tom is a cat. Mickey was a mouse.`).