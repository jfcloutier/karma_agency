/*
Cognition Actor support library.
*/

:- module(ca_support, [from_parent/2, about_belief/3, belief_value/2, same_belief_value/2, get_wellbeing/4, put_wellbeing/3, wellbeing_transfered/3, well_enough/1]).

:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(utils(logger)).

handled(message(Message, Source), State, State) :-
	log(debug, ca_support, "~@ is NOT handling message ~p from ~w", [self, Message, Source]).

% FOR DEBUGGING ONLY
handled(query(state), State, State).

handled(query(Query), State, unknown) :-
    log(debug, ca_support, "~@ is NOT handling query ~p given state ~p", [self, Query, State]).

% Remove from parents if applicable
handled(event(ca_terminated, _, Source), State, NewState) :-
    get_state(State, parents, Parents),
    member(Source, Parents),
    subtract(Parents, [Source], Parents1),
    put_state(State, parents, Parents1, NewState).

handled(event(Topic, Payload, Source), State, State) :-
    log(debug, ca_support, "~@ is NOT handling event ~p with ~p from ~w", [self, Topic, Payload, Source]).
    
from_parent(Source, State) :-
    get_state(State, parents, Parents),
    member(Source, Parents).

about_belief(PredictedBelief, State, Belief) :-
    PredictedBelief =.. [BeliefName, Object, _],
    get_state(State, beliefs, Beliefs),
    member(Belief, Beliefs),
    Belief =.. [BeliefName, Object, _].
    
belief_value(Belief, Value) :-
   Belief =.. [_, _, Value].

same_belief_value(PredictedValue, ActualValue-Tolerance) :-
    number(PredictedValue),
    number(ActualValue),
    !,
    abs(PredictedValue - ActualValue)=< Tolerance.

same_belief_value(Value, Value).

get_wellbeing(State, Fullness, Integrity, Engagement) :-
    get_state(State, wellbeing, Wellbeing),
	option(fullness(Fullness), Wellbeing),
	option(integrity(Integrity), Wellbeing),
	option(engagement(Engagement), Wellbeing).

put_wellbeing(State, UpdatedWellbeing, NewState) :-
    published(wellbeing_changed, UpdatedWellbeing),
	put_state(State, wellbeing, UpdatedWellbeing, NewState).

wellbeing_transfered(State, WellbeingTransfer, NewState) :-
    get_state(State, wellbeing, Wellbeing),
	option(fullness(Fullness), Wellbeing),
	option(integrity(Integrity), Wellbeing),
	option(engagement(Engagement), Wellbeing),
	option(fullness(FullnessTransfer), WellbeingTransfer),
	option(integrity(IntegrityTransfer), WellbeingTransfer),
	option(engagement(EngagementTransfer), WellbeingTransfer),
    Fullness1 is min(100, Fullness + FullnessTransfer),
    Integrity1 is min(100, Integrity + IntegrityTransfer),
    Engagement1 is min(100, Engagement + EngagementTransfer),
	put_state(State, wellbeing, [fullness = Fullness1, integrity = Integrity1, engagement = Engagement1], NewState).

% Well enough to do something if both fullness and integrity are > 0
well_enough(State) :-
	get_state(State, wellbeing, Wellbeing),
	option(fullness(Fullness), Wellbeing),
	Fullness > 0,
	option(integrity(Integrity), Wellbeing),
	Integrity > 0.
