/*

The feeling of hunger.

*/

:- module(hunger, []).

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(actor_utils)).

start(Supervisor) :-
    supervisor:start_child(
        Supervisor, 
        worker, 
        hunger, 
        [
            topics([]),
            init(hunger:init),
            handler(hunger:handle),
            terminate(hunger:terminate),
            restart(permanent)
        ]
    ).

init(State) :-
    log(info, hunger, 'Initiating'),
    empty_state(State).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, hunger, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, hunger, 'Handling query ~p in state ~p', [Query, State]).

terminate :-
    log(info, hunger, 'Terminating').