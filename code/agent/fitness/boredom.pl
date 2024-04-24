/*

The feeling of boredom.

*/

:- module(boredom, []).

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(actor_utils)).

start(Supervisor) :-
    supervisor:start_child(
        Supervisor, 
        worker, 
        boredom, 
        [
            topics([]),
            init(boredom:init),
            handler(boredom:handle),
            terminate(boredom:terminate),
            restart(permanent)
        ]
    ).

init(State) :-
    log(info, boredom, 'Initiating'),
    empty_state(State).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, boredom, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, boredom, 'Handling query ~p in state ~p', [Query, State]).

terminate :-
    log(info, boredom, 'Terminating').