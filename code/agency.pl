/*
Agency is the top level module.

It integrates

* Self-maintenance risks assessment and consequent feeling broadcasting (feelings)
* The dynamic collective of cognition actors (som)
* The interface to the body's effectors and detectors (body)
*/

:- module(agency, []).

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

start_supervisor(agency) :-
    supervisor:start(agency).
