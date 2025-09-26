/*
%% Start world and body servers
[load].
[load_tests].
run_tests(sensor_ca).
*/

:- begin_tests(sensor_ca, [setup(init_static_som), cleanup(terminate_som)]).

:- use_module(test_helper).

:- use_module(utils(logger)).
:- use_module(actors(supervisor)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(agency(agent)).
:- use_module(agency(som)).

:- set_log_level(info).

test(sensor_ca_in_som) :-
	query_answered(som, children, SOMChildren),
	assertion(SOMChildren \== unknown),
	findnsols(1,
		CA,
		(member(child(worker, CA),
			SOMChildren), query_answered(CA, type, sensor_ca)),
		L),
	assertion(L \== []).

test(all_sensor_cas_are_at_level_0) :-
    som:sensor_cas(SensorCAs),
	assertion(forall(member(SensorCA, SensorCAs), query_answered(SensorCA, level, 0))).

test(sensor_reading) :-
	SensorCA = 'sensor:light-in2:reflected',
	query_answered(som, children, SOMChildren),
	member(child(worker, SensorCA), SOMChildren),
	query_answered(SensorCA, reading, reading(Value, Tolerance, Timestamp)),
	assertion(number(Value)),
	assertion(number(Tolerance)),
	get_time(Now),
	Now >= Timestamp.

test(inaccurate_prediction) :-
	SensorCA = 'sensor:touch-in1:contact',
	SensorName = 'touch-in1',
	message_sent(SensorCA, adopted),
	query_answered(SensorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
	published(prediction, [experience = contact(SensorName, pressed)]),
	get_message(message(prediction_error(_, ActualValue), SensorCA)),
	assertion(ActualValue == released).

test(experience_acquired) :-
	SensorCA = 'sensor:light-in2:color',
	SensorName = 'light-in2',
	message_sent(SensorCA, adopted),
	query_answered(SensorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
	Prediction = [experience = color(SensorName, red)],
	published(prediction, Prediction),
	get_message(message(prediction_error(Prediction, ActualValue), SensorCA)),
	assertion(ActualValue \== red),
	query_answered(SensorCA, state, State),
	get_state(State, experiences, [Experience]),
	assertion(unifiable(Experience, color(SensorName, _-_), _)).

test(wellbeing_updated) :-
	% Pick a sensor CA and establish self as its parent
	SensorCA = 'sensor:light-in2:color',
	SensorName = 'light-in2',
	subscribed(wellbeing_changed),
	message_sent(SensorCA, adopted),
	query_answered(SensorCA, state, State),
	get_state(State, wellbeing, InitialWellbeing),
	% Get the sensor CA to read its sensor and message back a prediction error
	Prediction = [experience = color(SensorName, green)],
	published(prediction, Prediction),
	!,
	get_message(message(prediction_error(Prediction, Color), SensorCA)),
	% Check that the sensor CA published its update wellbeing to its parents
	get_message(event(wellbeing_changed, UpdatedWellbeing, SensorCA)),
	(Color == green -> 
		assert_wellbeing_changed(InitialWellbeing, UpdatedWellbeing, [fullness = >, integrity = <, engagement = >])
		;
		assert_wellbeing_changed(InitialWellbeing, UpdatedWellbeing, [fullness = <, integrity = <, engagement = >])).
:- end_tests(sensor_ca).
