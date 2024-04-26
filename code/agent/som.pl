/*
The agent's Society of Mind.

The SOM supervises the collective of cognition actors

*/

:- module(som, []).

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(som(effector)).
:- use_module(som(sensor)).
:- use_module(som(meta_ca)).

start(Parent, Sensors, Effectors) :-
    log(info, som, 'Starting SOM'),
    start_supervisor(Parent, Supervisor),
    initialize(Supervisor, Sensors, Effectors).

start_supervisor(Parent, som) :-
    supervisor:start_child(Parent, supervisor, som, [restart(transient)]).

initialize(Supervisor, Sensors, Effectors) :-
    log(info, som, 'Initializing SOM with sensors ~p and effectors ~p', [Sensors, Effectors]).
    % start_sensor_cognition_actors(Sensors),
    % start_effector_cognition_actors(Effectors),
    % start_meta_cognition_actor.