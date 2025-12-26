/*
Integrate current and past observations into experiences and assign goodness to each.

Analysis: count (0, 1, 2, 3, many) and trend (up, down, same)

-- From causal theory

Abduction: new named objects (other than named experiences from sensing, acting and analysis), 
           new named properties (between objects and values in domains),
           new named relations (between objects),
           new incompatibilities (between relations)

Induction: expected coincidences and consequences

Framing: endurance by default

*/

:- module(experience, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).


unit_of_work(CA, State, more(State, WellbeingDeltas)) :-
    member(N, [1,2,3,4,5,6,7,8,9]),
    sleep(0.02),
    wellbeing:empty_wellbeing(WellbeingDeltas),
    log(info, predict, "Phase plan more (~w) for CA ~w", [N, CA]).

unit_of_work(_, State, done(State, WellbeingDeltas)) :-
    wellbeing:empty_wellbeing(WellbeingDeltas).


% unit_of_work(CA, State, WorkStatus) can be undeterministic, resolving WorkStatus 
% to more(IntermediateState, WellbeingDeltas) or done(EndState, WellbeingDeltas) as last solution. 

% Add one experience at a time, starting with the most currently relevant ones.
% unit_of_work(CA, State, more(NewState, WellbeingDeltas)) :-
%     experience(CA, State, Experience),
%     acc_state(State, experience, Experience, NewState),
%     wellbeing:empty_wellbeing(WellbeingDeltas),
%     log(info, predict, "Phase experience for CA ~w added ~p", [CA, Experience]).

% unit_of_work(CA, State, done(State, WellbeingDeltas)) :-
%     wellbeing:empty_wellbeing(WellbeingDeltas),
%     log(info, experience, "Phase experience done for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]).

% % TODO
% % experience(CA, State, Experience)
% experience(_, _, _).
