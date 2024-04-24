/*

The feeling of pain.

*/

:- module(pain, []).

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(actor_utils)).

start(Supervisor) :-
    supervisor:start_child(
        Supervisor, 
        worker, 
        pain, 
        [
            topics([]),
            init(pain:init),
            handler(pain:handle),
            terminate(pain:terminate),
            restart(permanent)
        ]
    ).

init(State) :-
    log(info, pain, 'Initiating'),
    empty_state(State).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, pain, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, pain, 'Handling query ~p in state ~p', [Query, State]).

terminate :-
    log(info, pain, 'Terminating').