/*
An effector is an a priori cognition actor that communicates with a body effector to execute actions.
The body considers each possible action a device can take as defining a separate effector.
Same-device effectors are combined in one effector_ca.
*/

:- module(effector_ca, []).
:- use_module(actor_model(actor_utils)).

:- use_module(code(logger)).

name(Effector, Name) :-
    atomic_list_concat([effector, Effector.id, Effector.capabilities.action], ':', Name).

init(Options, State) :-
    log(info, effector_ca, 'Initiating with ~p', [Options]),
    empty_state(EmptyState),
    option(effectors(Effectors), Options),
    put_state(EmptyState, device, Effectors, State),
    send_message(start).

terminate :-
    log(warn, effector_ca, 'Terminating').

handle(message(Message, Source), State, State) :-
   log(info, effector_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, effector_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(Query), _, tbd) :-
    log(info, effector_ca, '~@ is NOT handling query ~p', [self, Query]).