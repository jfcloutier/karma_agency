/*
Cognition Actor support library.
*/

:- module(ca_support, [from_parent/2, prediction_handled/3, get_wellbeing/4, put_wellbeing/3, wellbeing_transfered/3, well_enough/1]).

:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(utils(logger)).

handled(message(Message, Source), State, State) :-
	log(debug, ca_support, "~@ is NOT handling message ~p from ~w", [self, Message, Source]).

handled(query(parents), State, Parents) :-
    get_state(State, parents, Parents).

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

% 'sensor:touch-in1:contact' is NOT handling event prediction with [experience=contact('touch-in1:contact',pressed)]
prediction_handled(PredictionPayload, Parent, State) :-
    log(info, ca_support, "Handling prediction ~p in state ~p", [PredictionPayload, State]),
	option(experience(PredictedExperience), PredictionPayload),
	about_experience(PredictedExperience, State, Experience),
	experience_value(PredictedExperience, PredictedValue),
	experience_value(Experience, ActualValue),
	(same_experience_value(PredictedValue, ActualValue) ->
		true;
        value_without_tolerance(ActualValue, Value),
		message_sent(Parent, prediction_error(PredictionPayload, Value))
	).	

about_experience(PredictedExperience, State, Experience) :-
    PredictedExperience =.. [ExperienceName, Object, _],
    get_state(State, experiences, Experiences),
    member(Experience, Experiences),
    Experience =.. [ExperienceName, Object, _].
    
experience_value(Experience, Value) :-
   Experience =.. [_, _, Value].

same_experience_value(Value1, Value2) :-
    number_with_tolerance(Value1, Val1-Tolerance1),
    number_with_tolerance(Value2, Val2-Tolerance2),
    !,
    Tolerance is Tolerance1 + Tolerance2,
    abs(Val2 - Val1) =< Tolerance.

same_experience_value(Value, Value).

number_with_tolerance(Value-Tolerance, Value-Tolerance) :- number(Value).

number_with_tolerance(Value, Value-0) :- number(Value).

value_without_tolerance(Value-_, Value) :- !.
value_without_tolerance(Value, Value).

get_wellbeing(State, Fullness, Integrity, Engagement) :-
    get_state(State, wellbeing, Wellbeing),
	option(fullness(Fullness), Wellbeing),
	option(integrity(Integrity), Wellbeing),
	option(engagement(Engagement), Wellbeing).

put_wellbeing(State, UpdatedWellbeing, NewState) :-
    log(info, ca_support, "~@ put wellbeing ~p", [self, UpdatedWellbeing]),
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
	Integrity > 0,
    log(debug, ca_support, "~@ is well enough", [self]).
