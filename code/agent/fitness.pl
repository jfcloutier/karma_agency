/*
Aggregation of events from the SOM into updated feelings broadcasted back to the SOM as signals of risks to homestasis.
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
