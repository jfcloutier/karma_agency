/*
The agent is the top level module.

It encompasses

* Homeostatic risks assessment and feeling broadcasting
* The evolving Society of Mind
* The interface to the body's effectors and detectors
*/

:- module(agent, []).

:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(som(som)).
:- use_module(fitness).
:- use_module(body).

start :-
    start_supervisor(Supervisor),
    supervisor:start_child(Supervisor, pubsub, [restart(transient)]),
    body:start(Supervisor),
    fitness:start(Supervisor),
    som:start(Supervisor).

start_supervisor(agent) :-
    supervisor:start(agent).
