/*
This is the top level module implementing a robot's agency.

It integrates services and actors:

* Body: A service providing an interface to real or virtual effectors and sensors
* Wellbeing: The supervisor of the wellbeing workers that do survival risk assessment and broadcast consequent feelings
* SOM: The supervisor of the dynamic collective of cognition and metacognition workers.
* PubSub: The actor via which agency actors subscribe to, publish and receive events
*/

/*
% Start karma_world server then karma_body server
[load].
[agent(agency), actor_model(supervisor), code(logger), actor_model(actor_utils), actor_model(pubsub)].
set_log_level(info).
agency:start('localhost:4000').
threads.
send_query('som', children, SOMChildren).
% send('sensor:ultrasonic-in4:distance', state).
send('tacho_motor-outA', state).
send_query('tacho_motor-outA', action_domain, Answer).
send_message('tacho_motor-outA', actuate(spin)).
send_message('tacho_motor-outA', actuate(reverse_spin)).

body:execute_actions('localhost:4000').

publish(prediction(distance), [value(130)]).
publish(prediction(distance), [value(0)]).
thread_get_message(Message).

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
:- use_module(som(meta_ca)).
:- use_module(wellbeing(engagement)).
:- use_module(wellbeing(fullness)).
:- use_module(wellbeing(integrity)).

start(BodyHost) :-
    log(info, agency, 'Starting agency'),
    body:capabilities(BodyHost, Sensors, Effectors),
    log(info, agency, 'Sensors: ~p', [Sensors]),
    log(info, agency, 'Effectors: ~p', [Effectors]),
    WellbeingChildren = [
        % Energy level
        worker(fullness, [topics([]), restart(permanent)]),
        % Physical integrity
        worker(integrity, [topics([]), restart(permanent)]),
        % Moving and learning?
        worker(engagement, [topics([]), restart(permanent)])
    ],
    AgencyChildren = [
        pubsub,
        supervisor(wellbeing, [children(WellbeingChildren), restart(permanent)]),
        supervisor(som, [restart(permanent)])
        ],
    supervisor:start(agency, [children(AgencyChildren)]),
    % Start the SOM with the sensor and effector CAs (at level 0)
    start_sensor_cas(Sensors),
    start_effector_cas(Effectors),
    % Start a metacognition actor at level 1
    meta_ca:name_from_level(1, Name),
    supervisor:start_worker_child(som, meta_ca, Name, [init([level(1)])]).

    start_sensor_cas([]).
    start_sensor_cas([Sensor | Others]) :-
        sensor_ca:name_from_sensor(Sensor, Name),
        supervisor:start_worker_child(som, sensor_ca, Name, [init([sensor(Sensor)])]),
        start_sensor_cas(Others).

    % The body presents each possible action by an actual effector as a separate effector capability.
    % We combine them into a single effector CA
    start_effector_cas([]).
    start_effector_cas([Effector | Others]) :-
        findall(Twin, (member(Twin, Others), Twin.id == Effector.id), Twins),
        supervisor:start_worker_child(som, effector_ca, Effector.id, [init([effectors([Effector |Twins])])]),
        subtract(Others, Twins, Rest),
        start_effector_cas(Rest).
    
