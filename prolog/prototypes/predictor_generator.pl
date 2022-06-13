% Generates the next predictor of a GM
:- module(predictor_generator, [better_predictor/4, complexity/2]).

:- use_module(library(clpfd)).
:- use_module(library(chr)).
:- use_module(sequence).
:- use_module(gm).
:- use_module(predictor).

%% Search constraints
:- chr_constraint candidate_predictor/2, best_candidate/2.

% Only keep the best rated candidate predictor
candidate_predictor(_Predictor1, Rating1) \ candidate_predictor(_Predictor2, Rating2) <=> Rating1 #>= Rating2 | true.

% Candidate extraction from the store
candidate_predictor(P, Rating)#passive, best_candidate(P, Rating) <=> true.

%% Search

better_predictor(GM, CurrentPredictor, NextPredictor, NextRating) :-
    timebox(MaxTime),
    rating(CurrentPredictor, Sequence, CurrentRating),
    candidate_predictor(CurrentPredictor,CurrentRating),
    enacted_sensory_sequence(GM, Sequence),
    % Generate as many candidate predictors as time allows
    find_predictors(GM, Sequence, MaxTime),
    % Extract candidate predictor from CHR store
    best_candidate(NextPredictor, NextRating).

rating(Predictor, Sequence, Rating) :-
    complexity(Predictor.static_rules, CSR),
    complexity(Predictor.causal_rules, CCR),
    complexity(Predictor.integrity_rules, CIR),
    Complexity is CSR + CCR + CIR,
    partial_coverage(Predictor, Sequence, PC),
    Rating is PC / Complexity.

% How complex an arbitrary term is.
complexity([], 0) :- !.
complexity([Element | OtherElements], Complexity) :-
    !,
    complexity(Element, Complexity1),
    complexity(OtherElements, Complexity2),
    Complexity is Complexity1 + Complexity2.

complexity(Term, Complexity) :-
    compound(Term),
    !,
    Term =.. List,
    complexity(List, Complexity).

complexity(_, 1).

partial_coverage(Predictor, Sequence, PC) :-
    Predictor.coverage(Coverage),
    length(Sequence, Length),
    PC is Coverage / Length.
    
find_predictors(GM, Sequence, MaxTime).

timebox(T) :-
    get_time(Now),
    max_predictor_search_time(MaxTime),
    T is Now + MaxTime.

