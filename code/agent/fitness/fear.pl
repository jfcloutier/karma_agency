/*

The feeling of fear.

*/

:- module(fear, []).

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(actor_utils)).

start(Supervisor) :-
    supervisor:start_child(
        Supervisor, 
        worker, 
        fear, 
        [
            topics([]),
            init(fear:init),
            handler(fear:handle),
            terminate(fear:terminate),
            restart(permanent)
        ]
    ).

init(State) :-
    log(info, fear, 'Initiating'),
    empty_state(State).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, fear, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, fear, 'Handling query ~p in state ~p', [Query, State]).

terminate :-
    log(info, fear, 'Terminating').