:- module(predictor,
          [ empty_predictor/1
          ]).

% predictor:empty_predictor(Predictor), C = Predictor.coverage().
empty_predictor(predictor{causal_rules:[], coverage:0, integrity_rules:[], static_rules:[], variables:[]}).

P.coverage():=C :-
    C=P.coverage.
P.variables():=V :-
    V=P.variables.
P.static_rules():=SR :-
    SR=P.static_rules.
P.causal_rules():=CR :-
    CR=P.causal_rules.
P.integrity_rules():=IR :-
    IR=P.integrity_rules.