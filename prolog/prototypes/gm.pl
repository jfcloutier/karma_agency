%% GM utilities
:- module(gm,
          [ current_predictor/2,
          max_predictor_search_time/1
          ]).

:- use_module(gms_db).
:- use_module(predictor_generator).
:- use_module(predictor).

% Extracts current predictor from code

% predictor(coverage(0), variables([]), static_rules([]), causal_rules([]), integrity_rules([]))
current_predictor(GM, Predictor) :-
    gm(GM),
    GM:has_predictor(Predictor),!.

current_predictor(_, Predictor) :-
     empty_predictor(Predictor).

next_predictor(GM, CurrentPredictor, Predictor) :-
    better_predictor(GM, CurrentPredictor, Predictor),
    !,
    update_with_predictor(GM, Predictor).

update_with_predictor(GM, Predictor) :-
    abolish(GM:has_predictor/1),
    assert(GM:has_predictor(Predictor)),
    extend_scope_for_predictor(GM, Predictor).

% TODO
extend_scope_for_predictor(GM, Predictor) :- true.


% 20 seconds max elpased allocated to finding a better predictor for a GM
max_predictor_search_time(20).