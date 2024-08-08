/*
Agency is the top level module.

It integrates

* Body: The interface to effectors and sensors
* Wellbeing: Self-maintenance risks assessment and consequent broadcasting of feelings
* SOM: The dynamic collective of cognition and metacognition actors. CAs are grown bottom-up and culled top-down.

Hierarchical SOM and agency

* A Level N + 1 CA 
    * models the (Level N) CAs in its umwelt but not vice-versa
        * umwelt CAs can't make sense of what's asked of/imposed on them (Froese's Irruption Theory?)
    * reduces the agency of its umwelt
        * restricts the causal theory search space of umwelt CAs
            * they can not disappear trends integrated in parent CA's beliefs (esp. trends involving abduced objects/predicates)
                * this imposes constraints on signatures of causal theories
                    * the choice of abduced objects/predicates is reduced
        * impose intents on Level N CAs to achieve its own goals
            * umwelt CAs decide how to fulfill imposed intents (they are not micromanaged, just directed)
            * they can't fulfill their own goals while fulfilling their parent CA's goals
                * they have fewer opportunity to intend their own goals and work on being/staying relevant
    * has a better chance than its umwelt to keep the agent alive 
        * it can detect the more abstract observation trends that are more likely to correlate with wellbeing trends
        * and thus its goals to validate/invalidate its beliefs (these trends) are likely more effective
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
    forall(
            (member(Sensor, Sensors), sensor_ca:name_from_sensor(Sensor, Name)),
            supervisor:start_worker_child(som, sensor_ca, Name, [init([sensor(Sensor)])])
          ),
    start_effectors(Effectors),
    % Start a metacognition actor at level 1
    meta_ca:name_from_level(1, Name),
    supervisor:start_worker_child(som, meta_ca, Name, [init([level(1)])]).


    start_effectors([]).
    start_effectors([Effector | Others]) :-
        findall(Twin, (member(Twin, Others), Twin.id == Effector.id), Twins),
        supervisor:start_worker_child(som, effector_ca, Effector.id, [init([effectors([Effector |Twins])])]),
        subtract(Others, Twins, Rest),
        start_effectors(Rest).
    
