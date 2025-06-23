/*
Cognition Actor support library.
*/

:- module(ca_support, [from_parent/2, about_belief/3, belief_value/2, same_belief_value/2]).

:- use_module(actors(actor_utils)).
:- use_module(utils(logger)).

handled(message(Message, Source), State, State) :-
	log(debug, ca_support, "~@ is NOT handling message ~p from ~w", [self, Message, Source]).

handled(query(Query), _, unknown) :-
    log(debug, ca_support, "~@ is NOT handling query ~p", [self, Query]).

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