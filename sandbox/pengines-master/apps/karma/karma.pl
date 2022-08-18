:- module(karma, [
    % Asserting
    assert_this_gm/1,
    assert_this_round/1,
    assert_in_round/2
    % Inferring
    ]).

:- dynamic this_gm/1, this_round/1, prior_round/2, belief/3, prediction/4, prediction_error/4.

%%% ASSERTIONS 

assert_this_gm(GM) :-
    assert(this_gm(GM)).

assert_this_round(Round) :-
    this_round(PriorRound),
    !,
    retractall(this_round(PriorRound)),
    assert(prior_round(PriorRound, Round)),
    assert(this_round(Round)).

% First round
assert_this_round(Round) :-
    assert(this_round(Round)).

assert_in_round(Round, Data) :-
    this_round(Round), % can only assert current round data
    Data =.. [Functor | Args],
    RoundData =.. [Functor, Round | Args],
    assert(RoundData).

%%% About asserted beliefs held, predictions and prediction errors sent/received by GM

round_belief(Round, Belief) :-
    belief(Round, Conjecture, About),
    Belief =.. [belief, Conjecture, About].

prediction_received(Round, Prediction) :-
    this_gm(GM),
    prediction(Round, Belief, GoalOrOpinion, ByGM),
    ByGM \== GM,
    Prediction =.. [prediction, Belief, GoalOrOpinion, ByGM].

prediction_made(Round, Prediction) :- 
    this_gm(GM),
    prediction(Round, Belief, GoalOrOpinion, GM),
    Prediction =.. [prediction, Belief, GoalOrOpinion, GM].

prediction_error_received(Round, PredictionError) :-
    this_gm(GM),
    prediction_error(Round, Prediction, ContrarianBelief, ByGM),
    ByGM \== GM,
    PredictionError =.. [prediction_error, Prediction, ContrarianBelief, ByGM].

prediction_error_made(Round, PredictionError) :-
    this_gm(GM),
    prediction_error(Round, Prediction, ContrarianBelief, GM),
    PredictionError =.. [prediction_error, Prediction, ContrarianBelief, GM].

%%% Inferring perceptions from predictions made and prediction errors received
perception(Round, )
  
%%% Inferring next round beliefs from this round's perceptions

%%% Inferring predictions in th