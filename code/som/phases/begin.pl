/*
The timeframe begins - diffuse wellbeing.

The CA calculates how much wellbeing it can diffuse:
    R = many relatives (parents and siblings) it has
    D(measure) = wellbeing(measure, self) / R

It then messages each relative to find their wellbeing(measure, Relative)

The CA sends to a relative D(measure) - wellbeing(measure, Relative) if > 0
*/

:- module(begin, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/wellbeing)).

% No work done before units of work
before_work(_, State, State).

% No work done after last unit of work
after_work(_, State, State).

% unit_of_work(CA, State, WorkStatus) by a phase can be non-deterministic, 
% resolving WorkStatus to more(IntermediateState, WellbeingDeltas), or it can be done(EndState, WellbeingDeltas) as the last or only solution. 

% No state change but wellbeing deltas from diffusion
unit_of_work(CA, State, done(State, WellbeingDeltas)) :-
    state{parents:Parents, umwelt:Umwelt, wellbeing:Wellbeing} :< State,
    append(Parents, Umwelt, Relatives),
    length(Relatives, Count),
    % Diffusable to each relative
    Diffusable = Wellbeing.div(Count),
    diffused(CA, Diffusable, Relatives, Diffused),
    WellbeingDeltas = Diffused.neg(),
    log(info, begin, "Phase begin done for CA ~w with wellbeing delta ~p", [CA, WellbeingDeltas]).

diffused(_, DiffusableWellbeing, [], DiffusableWellbeing).
diffused(CA, DiffusableWellbeing, [Relative | Rest], DiffusedWellbeing) :-
    diffused_to_relative(CA, DiffusableWellbeing, Relative, DiffusedWellbeing1),
    diffused(CA, DiffusableWellbeing, Rest, DiffusedWellbeing2),
    DiffusedWellbeing = DiffusedWellbeing1.add(DiffusedWellbeing2).

diffused_to_relative(CA, DiffusableWellbeing, Relative, DiffusedWellbeing) :-
    query_answered(Relative, wellbeing, Wellbeing),
    Wellbeing \== unknown,
    !,
    % What's diffused is the difference being what's available for diffusion and what's held (a positive value or forced to 0)
    % Not all of the wellbeing available for diffusion to another necessarily diffuses to it. Some or all may be untouched.
    DiffusedWellbeing = DiffusableWellbeing.sub(Wellbeing),
    message_sent(Relative, wellbeing_transfer(wellbeing{fullness:DiffusedWellbeing.fullness, integrity:DiffusedWellbeing.integrity, engagment:DiffusedWellbeing.engagement}), CA),
    !.

diffused_to_relative(CA, DiffusableWellbeing, Relative, DiffusableWellbeing) :-
    log(warn, begin, "Failed to diffuse wellbeing from ~w to ~w", [CA, Relative]).

