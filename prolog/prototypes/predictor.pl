:- module(predictor,
          [ empty_predictor/1
          ]).

% predictor:empty_predictor(Predictor), C = Predictor.coverage().
empty_predictor(predictor{coverage:0, scope: [], causal_rules:[], constraints:[], initial_conditions:[], static_rules:[], variables:[]}).

P.scope():=Scope :-
    Scope=P.scope.
P.coverage():=Coverage :-
    Coverage=P.coverage.
P.initial_conditions():=InitialConditions :-
    InitialConditions=P.initial_conditions.

P.variables():=Variables :-
    Variables=P.variables.
P.static_rules():=StaticRules :-
    StaticRules=P.static_rules.
P.causal_rules():=CausalRules :-
    CausalRules=P.causal_rules.
P.constraints():=Constraints :-
    Constraints=P.constraints.