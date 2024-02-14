/*
The agent's Society of Mind.

The SOM is an initial collection of processes

* The a priori cognition actors (detectors and actuators)
* The a priori, level 1, metacognition actor

*/

:- module(som, []).

:- use_module(actor_model(supervisor)).
:- use_module(body).
:- use_module(actuator).
:- use_module(detector).
:- use_module(meta_ca).

start(Parent) :-
    start_supervisor(Parent, Supervisor),
    body:effectors(Effectors),
    body:sensors(Sensors),
    forall(member(Effector, Effectors), actuator:start(Supervisor, Effector)),
    forall(member(Sensor, Sensors), detector:start(Supervisor, Sensor)),
    meta_ca:start(Supervisor).

start_supervisor(Parent, som) :-
    supervisor:start_child(Parent, supervisor, som, [restart(transient)]).