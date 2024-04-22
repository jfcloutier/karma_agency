/*
The agent's Society of Mind.

The SOM is an initial collection of processes

* The a priori cognition actors (sensors and effectors)
* The a priori, level 1, metacognition actor

*/

:- module(som, []).

:- use_module(actor_model(supervisor)).
:- use_module(body).
:- use_module(effector).
:- use_module(sensor).
:- use_module(meta_ca).

start(Parent) :-
    start_supervisor(Parent, Supervisor),
    body:effectors(Effectors),
    body:sensors(Sensors),
    forall(member(Effector, Effectors), effector:start(Supervisor, Effector)),
    forall(member(Sensor, Sensors), sensor:start(Supervisor, Sensor)),
    meta_ca:start(Supervisor).

start_supervisor(Parent, som) :-
    supervisor:start_child(Parent, supervisor, som, [restart(transient)]).