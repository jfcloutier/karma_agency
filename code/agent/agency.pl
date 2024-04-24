/*
Agency is the top level module.

It integrates

* Body: The interface to effectors and sensors
* Fitness: Self-maintenance risks assessment and consequent broadcasting of feelings
* SOM: The dynamic collective of cognition and metacognition actors
*/

/*
[load].
[agent(agency), code(logger)].
set_log_level(debug).
agency:start('localhost:4000').
agency:stop.
*/

:- module(agency, []).

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(agent(som)).
:- use_module(agent(body)).
:- use_module(agent(fitness)).
:- use_module(agent(som)).

start(BodyHost) :-
    log(info, agency, 'Starting agency'),
    start_supervisor(Supervisor),
    supervisor:start_child(Supervisor, pubsub, [restart(transient)]),
    body:capabilities(BodyHost, Sensors, Effectors),
    fitness:start(Supervisor),
    som:start(Supervisor, Sensors, Effectors).

stop :-
    supervisor:kill(agency).

start_supervisor(agency) :-
    supervisor:start(agency).
