/*
Meta-cognition actor.

A meta-cognition actor queries CAs about their
 * name
 * level
 * latency
 * belief_domain
 * action_domain

A meta-cognition actor listens to events about
 * ca_started, expecting empty payload 

A meta-cognition actor emits events
 * mca_started, [level: :level]

State:
    * level - 1..? - the level of the CAs it is responsible for
    * wards - names of the CAs under oversight


What a meta-ca does:

    * It ensures Level -1 coverage in the umwelts of its wards by adding level Level CAs to the SOM - TODO

*/

:-module(meta_ca, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).

name_from_level(Level, Name) :-
    atomic_list_concat([metaca, Level], "_", Name).

init(Options, State) :-
    log(info, meta_ca, "Initiating with ~p", [Options]),
    empty_state(EmptyState),
    option(level(Level), Options),
    put_state(EmptyState, [level-Level, wards-[]], State),
    subscribe(ca_started),
    publish(mca_started, [level(Level)]).

terminate :-
    log(warn, meta_ca, 'Terminating').

handle(message(Message, Source), State, State) :-
   log(info, meta_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(ca_started, _, Source), State, NewState) :-
    send_query(Source, level, Level),
    get_state(State, level, Level),
    get_state(State, wards, Wards),
    put_state(State, wards, [Source | Wards], NewState).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, meta_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(name), _, Name) :-
    self(Name).

handle(query(level), State, Level) :-
    get_state(State, level, Level).

handle(query(Query), _, unknown) :-
    log(info, meta_ca, '~@ is NOT handling query ~p', [self, Query]).
