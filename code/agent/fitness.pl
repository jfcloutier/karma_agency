/*
 Events from the SOM are aggregated and evaluated for their impact on fitness.
 Feelings are broadcasted to inform cognition actors of changes in risks to fitness.
*/

:- module(fitness, []).

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(fitness(pain)).
:- use_module(fitness(fear)).
:- use_module(fitness(hunger)).
:- use_module(fitness(boredom)).

start(Parent) :-
    log(info, fitness, 'Starting fitness'),
    start_supervisor(Parent, Supervisor),
    pain:start(Supervisor),
    fear:start(Supervisor),
    hunger:start(Supervisor),
    boredom:start(Supervisor).
    
start_supervisor(Parent, fitness) :-
    supervisor:start_child(Parent, supervisor, fitness, [restart(transient)]).
