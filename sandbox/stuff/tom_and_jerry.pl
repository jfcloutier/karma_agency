:- module(tom_and_jerry, [analyze/1]).

:- use_module(library(chr)).

:- chr_constraint species_of/2, food_for/2.

%% Constraints

an_individual_belongs_to_only_one_species @ species_of(N, C1) \ species_of(N, C2) <=> C1 = C2.

a_food_is_eaten_by_only_one_species @ food_for(C, C1) \ food_for(C, C2) <=> C1 = C2.

%% Grammar rules

text --> [] ; sentence, maybe_space, text.

sentence --> name(N), space, (`is`;`was`), space, (`a` ; `an`), space, name(C), stop, {species_of(N, C)}.
sentence --> name(N1), space, `eats`, space, name(N2), stop, {species_of(N1, C1), species_of(N2, C2), food_for(C2, C1)}.

stop --> `.`.
stop --> `,`.

space --> ` ` ; ` `, space.

maybe_space --> [] | space.

name(N) --> [Char], {letter(Char), string_codes(N, [Char])}.
name(N) --> [Char | More], {letters([Char | More]), string_codes(N, [Char | More])}.

letter(Char) :- Char > 64 , Char < 122.

letters([]).
letters([Char | Rest]) :- letter(Char), letters(Rest).

%% Query

analyze(Input) :- phrase(text, Input).

% ['sandbox/stuff/tom_and_jerry'].

% analyze(`Tom eats Jerry, Jerry was a mouse. Tom is a cat.`).
% analyze(`Tom eats Jerry, Jerry was a mouse. Tom is a cat. Garfield eats Mickey. Mickey was a mouse.`).
% analyze(`Tom eats Jerry, Jerry was a mouse. Tom is a cat. Garfield is a cat. Tom eats Garfield.`).
% analyze(`Tom eats Jerry, Jerry was a mouse. Tom is a cat. Mickey is a mouse. Jerry eats Mickey.`).