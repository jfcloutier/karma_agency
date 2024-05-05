/*
Agency is the top level module.

It integrates

* Body: The interface to effectors and sensors
* Fitness: Self-maintenance risks assessment and consequent broadcasting of feelings
* SOM: The dynamic collective of cognition and metacognition actors
*/

/*
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
:- use_module(agent(som)).
:- use_module(agent(body)).
:- use_module(agent(fitness)).
:- use_module(agent(som)).

start(BodyHost) :-
    log(info, agency, 'Starting agency'),
    body:capabilities(BodyHost, Sensors, Effectors),
    FitnessChildren = [
        worker(fullness, fullness, [topics([]), restart(permanent)]),
        worker(integrity, integrity, [topics([]), restart(permanent)]),
        worker(competence, competence, [topics([]), restart(permanent)]),
        worker(engagement, engagement, [topics([]), restart(permanent)])
    ],
    AgencyChildren = [
        pubsub,
        supervisor(fitness, [children(FitnessChildren),restart(permanent)]),
        % TODO - Support passing args to init
        worker(som, [init([Sensors, Effectors]), restart(transient)])
        ],
    supervisor:start(agency, [children(AgencyChildren)]).

