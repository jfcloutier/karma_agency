/*
Agency is the top level module.

It integrates

* Body: The interface to effectors and sensors
* Fitness: Self-maintenance risks assessment and consequent broadcasting of feelings
* SOM: The dynamic collective of cognition and metacognition actors
*/

/*
% Start karma world and body servers
[load].
[agent(agency), actor_model(supervisor), code(logger)].
set_log_level(debug).
agency:start('localhost:4000').
supervisor:stop(agency).
*/

:- module(agency, []).

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(agent(body)).
:- use_module(som(sensor_ca)).
:- use_module(som(effector_ca)).
:- use_module(fitness(competence)).
:- use_module(fitness(engagement)).
:- use_module(fitness(fullness)).
:- use_module(fitness(integrity)).

start(BodyHost) :-
    log(info, agency, 'Starting agency'),
    body:capabilities(BodyHost, Sensors, Effectors),
    log(info, agency, 'Sensors: ~p', [Sensors]),
    log(info, agency, 'Effectors: ~p', [Effectors]),
    FitnessChildren = [
        worker(fullness, [topics([]), restart(permanent)]),
        worker(integrity, [topics([]), restart(permanent)]),
        worker(competence, [topics([]), restart(permanent)]),
        worker(engagement, [topics([]), restart(permanent)])
    ],
    AgencyChildren = [
        pubsub,
        supervisor(fitness, [children(FitnessChildren), restart(permanent)]),
        supervisor(som, [restart(permanent)])
        ],
    supervisor:start(agency, [children(AgencyChildren)]),
    % Start the SOM with sensor and effector CAs
    forall(
            (member(Sensor, Sensors), sensor_ca:name(Sensor, Name)),
            supervisor:start_worker_child(som, sensor_ca, Name, [init([device(Sensor)])])
          ),
    forall(
            (member(Effector, Effectors), effector_ca:name(Effector, Name)),
            supervisor:start_worker_child(som, effector_ca, Name, [init([device(Effector)])])
          ).
