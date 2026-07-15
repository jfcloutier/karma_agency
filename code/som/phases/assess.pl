/*
Abandon intent if 
  * it is no longer relevant (the experience to impact is gone)
  * it is stalled (not executing or executed for N timeframes)
  * then drop any plan for it and let umwelt know the intent is abandoned

Abandon a directive if
  * it is no longer relevant or is stalled
  * then drop any plan for it and let the parents know that the directive cannot be executed

Abandon a plan if
  * it is stalled 

Over all timeframes
  * check if executed goal states have been achieved (experience impacts are now realized)
  * if achieved, score associated executed plans
    * the closer in time to goal achievement, the higher the score

Evaluate causal theory
  * If none and there's enough history (timeframe count > N), request one 
  * If too many prediction errors from applying the current causal theory, request a new one

Decide what happens next to the dynamic CA
  * apoptosis if too old (max trimeframes reached) or wellbeing is too low in any dimension and not trending up
  * else ask SOM whether to replicate, divide or do neither

*/

:- module(assess, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% No work done before units of work
before_work(_, _, [], WellbeingDelta) :-
    wellbeing:empty_wellbeing(WellbeingDelta).


% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(StateDeltas, WellbeingDelta) or done(StateDeltas, WellbeingDelta) as last solution. 
unit_of_work(CA, State, done(StateDeltas, WellbeingDelta)) :-
    staying_alive(State, Alive),
    StateDeltas = [alive=Alive],
    wellbeing:empty_wellbeing(WellbeingDelta),
    log(info, assess, "Phase assess ended for CA ~w", [CA]).

staying_alive(State, Alive) :-
    get_state(State, timeframe_count, Count),
    get_state(State, settings, Settings),
    option(max_timeframes(Max), Settings, infinity),
    ((Max \== infinity, Count >= Max) -> Alive = false ; Alive = true).