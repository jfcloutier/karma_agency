/*
Agency is the top level module.

It integrates

* Body: The interface to effectors and sensors
* Fitness: Self-maintenance risks assessment and consequent broadcasting of feelings
* SOM: The dynamic collective of cognition and metacognition actors
*/

/*
% Start karma_world server then karma_body server
[load].
[agent(agency), actor_model(supervisor), code(logger), actor_model(actor_utils), actor_model(pubsub)].
set_log_level(debug).
agency:start('localhost:4000').
threads.
% send('sensor:ultrasonic-in4:distance', state).
send('tacho_motor-outA', state).
send_query('tacho_motor-outA', action_domain, Answer).
send_message('tacho_motor-outA', actuate(spin)).
send_message('tacho_motor-outA', actuate(reverse_spin)).

body:execute_actions('localhost:4000').

% publish(prediction(distance), 130).

supervisor:stop(agency).
threads.
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
            supervisor:start_worker_child(som, sensor_ca, Name, [init([sensor(Sensor)])])
          ),
    start_effectors(Effectors).

    start_effectors([]).
    start_effectors([Effector | Others]) :-
        findall(Twin, (member(Twin, Others), Twin.id == Effector.id), Twins),
        supervisor:start_worker_child(som, effector_ca, Effector.id, [init([effectors([Effector |Twins])])]),
        subtract(Others, Twins, Rest),
        start_effectors(Rest).