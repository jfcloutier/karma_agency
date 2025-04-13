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
[agent(agency), actor_model(supervisor), code(logger), actor_model(actor_utils), actor_model(pubsub)].
set_log_level(info).
agency:started('localhost:4000').
threads.
query_sent('som', children, SOMChildren).
query_sent('effector:tacho_motor-outA', action_domain, Answer).
message_sent('effector:tacho_motor-outA', actuated(spin)).
message_sent('effector:tacho_motor-outA', actuated(reverse_spin)).

body:execute_actions('localhost:4000').

publish(prediction(distance), [value(130)]).
publish(prediction(distance), [value(0)]).
thread_get_message(Message).

supervisor:stopped(agency, 60).
threads.
*/

:- module(agency, []). 

:- use_module(code(logger)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(pubsub)).
:- use_module(agent(body)).
:- use_module(som(sensor_ca)).
:- use_module(som(effector_ca)).

started(BodyHost) :-
	log(info, agency, 'Starting agency'),
	body : capabilities(BodyHost, Sensors, Effectors),
	log(info, agency, 'Sensors: ~p', [Sensors]),
	log(info, agency, 'Effectors: ~p', [Effectors]),
	AgencyChildren = [
		pubsub,
		% Start the SOM supervisor with no children yet
		supervisor(som,
			[restarted(permanent)])],
	% Start agency - the top supervisor
	supervisor : started(agency,
		[children(AgencyChildren)]),
	sensor_cas_started(Sensors),
	effector_cas_started(Effectors),
	level_one_ca_started.

    % Starting sensor and effector CAs
	sensor_cas_started([]).

	sensor_cas_started([Sensor|Others]) :-
	sensor_ca : name_from_sensor(Sensor, Name),
	supervisor : worker_child_started(som,
		sensor_ca,
		Name,
		[init([sensor(Sensor)])]),
	sensor_cas_started(Others).
	
	% The body presents each possible action by an actual effector as a separate effector capability.
	% We combine them into a single effector CA
	effector_cas_started([]).
	
	effector_cas_started([Effector|Others]) :-
	findall(Twin,
		(member(Twin, Others),
			Twin.id == Effector.id),
		Twins),
	effector_ca : name_from_effector(Effector, Name),
	supervisor : worker_child_started(som,
		effector_ca,
		Name,
		[
				init([
					effectors([Effector|Twins])])]),
	subtract(Others, Twins, Rest),
	effector_cas_started(Rest).
	
	% TODO
	% Create the first CA capable of mitosis
	level_one_ca_started.
