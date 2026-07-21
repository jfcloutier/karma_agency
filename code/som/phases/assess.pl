/*
Abandon intent if 
  * it is no longer relevant (the experience to be impacted is gone)
  * it is stalled (not executing/executed for N timeframes)
  * then drop any plan for it and let umwelt know the intent is abandoned

Abandon a directive if
  * it is no longer relevant or it is stalled
  * then drop any plan for it and let the parents know that the directive cannot be executed

Abandon a plan if
  * it is stalled 

Score plans
  * over all timeframes, check if goal states went from executed to achieved (have targeted experience impacts been realized?)
  * if achieved, score associated executed plans
    * the closer in time to goal achievement, the higher the (correlation) score

Evaluate causal theory
  * If none and there's enough history (timeframe count > N), request one from the Apperception Engine
* If too many prediction errors from applying the current causal theory, request a new one (hold on to the old ones)

Diffuse wellbeing
  * broadcast wellbeing status (parents and umwelt are listeners)
  * decide how much wellbeing to transfer to which parents and umwelt CAs
    * from reviewing wellbeing status events received
    * and own wellbeing reserves + depletion rates (they modulate generosity)
  * send messages transfering wellbeing to needy parents and/or umwelt CAs
    * clear received wellbeing status events

Accept life event
  * ask SOM what's the next life event: apoptosis, replication, division or none
  * realize it, unless none
    * apoptosis distributes fullness to parents and umwelt
    * replication/division divides fullness equally, integrity is copied, engagement starts full for new CAs
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