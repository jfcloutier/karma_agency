/*
Agency is the top level module.

It integrates

* Body: The interface to effectors and detectors
* Fitness: Self-maintenance risks assessment and consequent broadcasting of feelings
* SOM: The dynamic collective of cognition and metacognition actors
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
    body:capabilities(Sensors, Effectors),
    fitness:start(Supervisor),
    som:start(Supervisor, Sensors, Effectors).

start_supervisor(agency) :-
    supervisor:start(agency).
