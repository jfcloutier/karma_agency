/*
This is the top level module implementing a robot's agency.

It integrates services and actors:

* Body: A service providing an interface to real or virtual effectors and sensors
* SOM: The supervisor of the dynamic collective of cognition actors.
* PubSub: The actor via which agency actors subscribe to, publish and receive events
*/

/*
% Start karma_world server then karma_body server
[load].
[code(agency), actors(supervisor), utils(logger), actors(actor_utils), actors(pubsub)].
set_log_level(info).
agency:started('localhost:4000').
threads.
query_answered('som', children, SOMChildren).
query_answered('effector:tacho_motor-outA', action_domain, Answer).
message_sent('effector:tacho_motor-outA', actuated(spin)).
message_sent('effector:tacho_motor-outA', actuated(reverse_spin)).

body:execute_actions('localhost:4000').

published(prediction(distance), [value(130)]).
published(prediction(distance), [value(0)]).
thread_get_message(Message).

supervisor:stopped(agency, 60).
threads.
*/

:- module(agency, []). 

:- use_module(utils(logger)).
:- use_module(som(som)).
:- use_module(actors(supervisor)).
:- use_module(code(body)).


%! started(+BodyHost) is det
% the agent is started on a body referenced by a url
started(BodyHost) :-
	log(info, agency, 'Starting agency'),
	body : capabilities(BodyHost, Sensors, Effectors),
	log(info, agency, 'Sensors: ~p', [Sensors]),
	log(info, agency, 'Effectors: ~p', [Effectors]),
	som : started(Sensors, Effectors).
