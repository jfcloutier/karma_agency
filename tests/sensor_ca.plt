/*
%% Start world and body servers
[load].
[load_tests].
run_tests(sensor_ca).
*/

:- begin_tests(sensor_ca, [setup(init_som), cleanup(terminate_som)]).

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
	query_answered(som, children, SOMChildren),
	member(child(worker, SensorCA), SOMChildren),
	SensorCA = 'sensor:light-in2:reflected',
	query_answered(SensorCA, reading, reading(Value, Tolerance, Timestamp)),
	assertion(number(Value)),
	assertion(number(Tolerance)),
	get_time(Now),
	Now >= Timestamp.

test(inaccurate_prediction) :-
	SensorCA = 'sensor:touch-in1:contact',
	message_sent(SensorCA, adopted),
	query_answered(SensorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
	published(prediction, [belief = sensed(contact, pressed)]),
	thread_get_message(message(prediction_error(Prediction, ActualValue-_), From)),
	format("@@@ Received prediction error from ~w to prediction ~p with actual value ~p~n", [From, Prediction, ActualValue]),
	atomic_list_concat(Atoms, :, From),
	assertion(member(sensor, Atoms)),
	assertion(member(contact, Atoms)),
	assertion(ActualValue == released).

test(belief_acquired) :-
	SensorCA = 'sensor:light-in2:color',
	message_sent(SensorCA, adopted),
	query_answered(SensorCA, parents, Parents),
	thread_self(Self),
	assertion(member(Self, Parents)),
	published(prediction, [belief = sensed(color, red)]),
	thread_get_message(message(prediction_error(Prediction, ActualValue-_), From)),
	format("@@@ Received prediction error from ~w to prediction ~p with actual value ~p~n", [From, Prediction, ActualValue]),
	query_answered(From, state, State),
	% put_state(State1, beliefs, [sensed(SenseName, Value-Tolerance)],  NewState).
	format("@@@ State is ~p~n", [State]),
	get_state(State, beliefs, [Belief]),
	assertion(unifiable(Belief, sensed(color, _-_), _)).

test(wellbeing_updated) :-
	% Pick a sensor CA and establish self as its parent
	SensorCA = 'sensor:light-in2:color',
	subscribed(wellbeing_changed),
	message_sent(SensorCA, adopted),
	query_answered(SensorCA, state, State),
	format("@@@ State is ~p~n", [State]),
	% wellbeing:[fullness=99,integrity=99,engagement=1]}
	get_state(State, wellbeing, Wellbeing),
	option(fullness(Fullness), Wellbeing),
	option(integrity(Integrity), Wellbeing),
	option(engagement(Engagement), Wellbeing),
	% Get the sensor CA to read its sensor and message back a prediction error
	Prediction = [belief = sensed(color, green)],
	published(prediction, Prediction),
	!,
	thread_get_message(message(prediction_error(Prediction, Color-_), Source)),
	format("@@@ Got prediction error ~w from ~w~n", [Color, Source]),
	% Check that the sensor CA published its update wellbeing to its parents
	thread_get_message(event(wellbeing_changed, UpdatedWellbeing, SensorCA)),
	format("@@@ Updated wellbeing is ~p~n", [UpdatedWellbeing]),
	% Verify that the wellbeing has changed
	query_answered(SensorCA, state, State1),
	format("@@@ Updated state is ~p~n", [State1]),
	get_state(State1, wellbeing, Wellbeing1),
	option(fullness(Fullness1), Wellbeing1),
	option(integrity(Integrity1), Wellbeing1),
	option(engagement(Engagement1), Wellbeing1),
	(Color == green -> assertion(Fullness1 >= Fullness); assertion(Fullness1 < Fullness)),
	assertion(Integrity1 < Integrity),
	assertion(Engagement1 > Engagement),
	option(fullness(UpdatedFullness), UpdatedWellbeing),
	option(integrity(UpdatedIntegrity), UpdatedWellbeing),
	option(engagement(UpdatedEngagement), UpdatedWellbeing),
	assertion(UpdatedFullness == Fullness1),
	assertion(UpdatedIntegrity == Integrity1),
	assertion(UpdatedEngagement == Engagement1).

:- end_tests(sensor_ca).

init_som :-
	agent : started('localhost:4000').

terminate_som :-
	supervisor : stopped(agency, 60).