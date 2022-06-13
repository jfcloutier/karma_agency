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
    GM:has_predictor(Predictor),! ; empty_predictor(Predictor).

next_predictor(GM, CurrentPredictor, NextPredictor) :-
    better_predictor(GM, CurrentPredictor, NextPredictor),
    !.

next_predictor(GM, CurrentPredictor, CurrentPredictor).

% 20 seconds max elpased allocated to finding a better predictor for a GM
max_predictor_search_time(20).