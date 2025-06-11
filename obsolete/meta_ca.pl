/*

Meta-cognition actor (meta-CA)

A meta-CA adds and removes cognition actors (CAs) at the level of the SOM it manages.

Events:

* In
  * fullness, [level(0..1)]
  * integrity, [level{0..1)]
  * engagement, [level(0..1)]
  * ca_started, [level(Level)]
  * ca_terminated, [level(Level)]

* Out
  * meta_ca_started, [level(Level)]
  * meta_ca_terminated, [level(Level)]

* Queries
  * type - meta-ca
  * level - Integer > 0
  * wards - [CAName, ...]
    
Thread locals:
    * level - integer (duplicated in thread local where it is needed for termination)
	* timer - the curator timer

State:
    * wards - ward CAs, or unknown if they need to be discovered from querying the SOM
	* meta_ca_below - the meta_ca one level down, if level > 0
	* sensor_count, effector_count - if level == 0
    * busy - true|false - busy while curating - we don't want to start curating while already curating
	* complete - whether coverage is complete

*/

:-module(meta_ca, []).

:- use_module(actors(actor_utils)).
:- use_module(utils(logger)).
:- use_module(actors(pubsub)).
:- use_module(actors(supervisor)).
:- use_module(actors(worker)).
:- use_module(actors(timer)).
:- use_module(agency(som/sensor_ca)).
:- use_module(agency(som/effector_ca)).
:- use_module(agency(som/ca)).
:- use_module(actors(task)).

% Static thread state
:- thread_local level/1, timer/1.

name_from_level(Level, Name) :-
	atomic_list_concat([metaca, level, Level], ":", Name).

% A meta-cognition actor re-evaluates the state of its layer of the SOM every (2 ** Level) * Level seconds

latency(Level, Latency) :-
	Latency is (2**Level)*Level.

%% Worker

init(Options, State) :-
	log(info, meta_ca, "Initiating ~@ with ~p", [self, Options]), 
	empty_state(EmptyState), 
	option(
		level(Level), Options), 
	assert(
		level(Level)), 
	subscribe_all([ca_started, ca_terminated, meta_ca_started, meta_ca_terminated]), 
	latency(Level, Latency), 
	self(Name), 
	(
		Level == 0 ->
			(
			option(
				sensors(Sensors), Options), 
			option(
				effectors(Effectors), Options), 
			start_sensor_cas(Sensors, SensorCount), 
			start_effector_cas(Effectors, EffectorCount), 
			maybe_start_meta_ca_above(Name, Level), 
			put_state(EmptyState, [level - Level, sensor_count - SensorCount, effector_count - EffectorCount, wards - [], busy - false, complete - true], State));
		(
			option(
				meta_ca_below(MetaCABelow), Options), 
			send_at_interval(Name, curator, 
				message(curate, Name), Latency, Timer), 
			assert(
				timer(Timer)), 
			put_state(EmptyState, [level - Level, meta_ca_below - MetaCABelow, wards - [], busy - false, complete - false], State))), 
	published(meta_ca_started, 
		[level(Level)]).

% Start meta_ca above Name which is at level Level, if not already started

maybe_start_meta_ca_above(Name, Level) :-
	LevelAbove is Level + 1, 
	meta_ca : name_from_level(LevelAbove, MetaCAAbove), 
	(
		 \+ is_thread(MetaCAAbove)
		 ->
				supervisor : start_worker_child(som, meta_ca, MetaCAAbove, 
			[init(
				[level(LevelAbove), 
				meta_ca_below(Name)])]);
				true).

process_signal(control(stop)) :-
	worker : stop.

% Called on thread exit
terminate :-
	log(warn, meta_ca, "~@ terminating", [self]), 
	timer(Timer), 
	timer : stop(Timer), 
	level(Level), 
	published(meta_ca_terminated, 
		[level(Level)]), 
	log(warn, meta_ca, "Terminated ~@", [self]).

handle(message(curate, _), State, State) :-
	get_state(State, busy, false), !, 
	curate(State).

handle(message(curate, _), State, State) :-
	log(debug, meta_ca, "Skipping curating because busy").

handle(message(busy(Busy), _), State, NewState) :-
	put_state(State, busy, Busy, NewState).

handle(message(Message, Source), State, State) :-
	log(debug, meta_ca, "~@ is NOT handling message ~p from ~w", [self, Message, Source]).

% In case the meta-ca below is restarted
handle(event(meta_ca_started, Payload, MetaCA), State, NewState) :-
	get_state(State, level, Level),
	LevelBelow is Level - 1, 
	option(level(LevelBelow), Payload), 
	!, 
	put_state(State, meta_ca_below, MetaCA, NewState).

% Check if the level is now complete and remember it

handle(event(ca_started, Payload, CA), State, NewState) :-
	option(
		level(Level), Payload), 
	get_state(State, level, Level), !, 
	get_state(State, wards, Wards), 
	put_state(State, wards, [CA|Wards], State1), 
	(
		level_is_complete(State1)
		 ->
				put_state(State1, complete, true, NewState);
				NewState = State1).

% Mark level as not complete if a CA was added at the level below
handle(event(ca_started, Payload, _), State, NewState) :-
	get_state(State, level, Level),
	LevelBelow is Level - 1, 
	option(level(LevelBelow), Payload), !, 
	put_state(State, complete, false, NewState).

handle(event(ca_terminated, Payload, CA), State, NewState) :-
	option(
		level(Level), Payload), 
	get_state(State, level, Level), !, 
	get_state(State, wards, Wards), 
	delete(Wards, CA, RemainingWards), 
	put_state(State, wards, RemainingWards, NewState).

handle(event(Topic, Payload, Source), State, State) :-
	log(debug, meta_ca, "~@ is NOT handling event ~w, with payload ~p from ~w)", [self, Topic, Payload, Source]).

handle(query(name), _, Name) :-
	self(Name).

handle(query(type), _, meta_ca).

handle(query(level), State, Level) :-
	get_state(State, level, Level).

handle(query(wards), State, Wards) :-
	get_state(State, wards, Wards).

% Level 0 - all sensor and effector CA are wards

handle(query(is_complete), State, true) :-
	get_state(State, level, 0), 
	get_state(State, sensor_count, SensorCount), 
	get_state(State, effector_count, EffectorCount), 
	get_state(State, wards, Wards), 
	length(Wards, WardCount), !, WardCount is SensorCount + EffectorCount.

handle(query(is_complete), State, true) :-
	get_state(State, level, Level), Level \= 0, 
	level_is_complete(State), !.

handle(query(is_complete), _, false).

handle(query(Query), _, unknown) :-
	log(debug, meta_ca, "~@ is NOT handling query ~p", [self, Query]).

%% Level 0

start_sensor_cas([], 0).

start_sensor_cas([Sensor|Others], SensorCount) :-
	sensor_ca : name_from_sensor(Sensor, Name), 
	supervisor : start_worker_child(som, sensor_ca, Name, 
		[init(
			[sensor(Sensor)])]), 
	start_sensor_cas(Others, OtherCount), SensorCount is OtherCount + 1.

% The body presents each possible action by an actual effector as a separate effector capability.
% We combine them into a single effector CA
start_effector_cas([], 0).

start_effector_cas([Effector|Others], EffectorCount) :-
	findall(Twin, 
		(
			member(Twin, Others), Twin.id == Effector.id), Twins), 
	effector_ca : name_from_effector(Effector, Name), 
	supervisor : start_worker_child(som, effector_ca, Name, 
		[init(
			[effectors([Effector|Twins])])]), 
	subtract(Others, Twins, Rest), 
	start_effector_cas(Rest, RestCount), EffectorCount is RestCount + 1.

%% Curating

% Curate by 
%   adding a ward CA to increase coverage (if needed)
%   and/or retiring a useless ward CA (if needed)

curate(State) :-
	send_message(
		busy(true)), 
	self(Name), % Curate in a task to free up the meta_ca's to process other messages
	
	do_and_then(
		meta_ca : do_curate(Name, State), 
		send_message(Name, 
			busy(false))).

%% Run in task thread

do_curate(Name, State) :-
	get_state(State, level, Level), 
	log(debug, meta_ca, "~@ is curating level ~w", [self, Level]), 
	cull(Name, State), 
	grow(Name, State).

% For each ward that is not dependent on by a CA at a higher level,
% evaluate if it is persistently useless.
% If so, terminate it.
% TODO
cull(_, _).

% Add a CA to the metaCA's level L if leve L - 1 is complete and level L is not.

grow(Name, State) :-
	get_state(State, level, Level), 
	get_state(State, meta_ca_below, MetaCABelow), 
	log(debug, meta_ca, "~w is growing level ~w ", [Name, Level]), 
	level_below_is_complete(State, MetaCABelow), 
	\+ level_is_complete(State), !, 
	log(info, meta_ca, "Adding new ca at level ~w", [Level]), 
	add_ca(State, NewCA), 
	log(debug, meta_ca, "~@ added CA ~p", [self, NewCA]).

grow(Name, State) :-
	level_is_complete(State),
	level_is_mature(State), !, 
	get_state(State, level, Level), 
	maybe_start_meta_ca_above(Name, Level).

grow(_, _).

level_is_complete(State) :-
	get_state(State, complete, true), !.

level_is_complete(State) :-
	cas_below(State, CASBelow), 
	get_state(State, level, Level), 
	log(debug, meta_ca, "Level ~w has umwelt-able CAs ~p", [Level, CASBelow]), 
	get_state(State, wards, WardCAs), 
	log(debug, meta_ca, "Level ~w has wards CAs ~p", [Level, WardCAs]), 
	all_covered_by(CASBelow, WardCAs).

cas_below(State, CASBelow) :-
	get_state(State, meta_ca_below, MetaCABelow), 
	send_query(MetaCABelow, wards, CASBelow).

level_below_is_complete(State, _) :-
	get_state(State, level, 1), !.
level_below_is_complete(State, MetaCABelow) :-
	get_state(State, meta_ca_below, MetaCABelow), 
	send_query(MetaCABelow, is_complete, true).

all_covered_by([], _).

all_covered_by([UmweltCA|Others], WardCAs) :-
	covered_by_any(UmweltCA, WardCAs), !, 
	all_covered_by(Others, WardCAs).

covered_by_any(UmweltCA, WardCAs) :-
	member(CA, WardCAs), 
	ca : umwelt(CA, Umwelt), 
	member(UmweltCA, Umwelt).

% Level 0 is always mature
level_is_mature(State) :-
	get_state(State, level, 0).
% TODO
level_is_mature(_) :-
	fail.

% If adding a CA at level 1, always include all effector_ca's in its umwelt (to ensure all CAs transitively have access to all effectors)
% Then assemble an umwelt of at least 1 or more non-covered CA and 0 or more covered CAs from level L - 1 (to always grow coverage and maybe create umwelt overlap)
% Create a CA with that umwelt at level L

add_ca(State, CAName) :-
	get_state(State, level, Level), 
	ca : name_from_level(Level, CAName), 
	pick_umwelt(State, Umwelt), 
	supervisor : start_worker_child(som, ca, CAName, 
		[init(
			[level(Level), 
			umwelt(Umwelt)])]), 
	log(info, meta_ca, "Added new CA ~w at level ~w with umwelt ~p", [CAName, Level, Umwelt]).

pick_umwelt(State, Umwelt) :-
	get_state(State, level, Level), 
	effector_cas(Level, EffectorCAs), 
	pick_uncovered_cas(State, EffectorCAs, UncoveredCAs), 
	pick_covered_cas(State, EffectorCAs, CoveredCAs), 
	flatten([EffectorCAs, UncoveredCAs, CoveredCAs], Umwelt).

effector_cas(1, EffectorCAs) :-
	meta_ca : name_from_level(0, MetaCA0), 
	send_query(MetaCA0, wards, Wards), 
	findall(Name, 
		(
			member(Name, Wards), 
			type_of(Name, effector)), EffectorCAs).

effector_cas(_, []).

pick_uncovered_cas(State, EffectorCAs, UncoveredCAs) :-
	get_state(State, level, Level), 
	get_state(State, wards, Wards), 
	UmweltLevel is Level - 1, 
	level_cas(UmweltLevel, CAs), 
	findall(CA, 
		(
			member(CA, CAs), 
			 \+ member(CA, EffectorCAs), 
			 \+ covered_by_any(CA, Wards)), AllUncoveredCAs), 
	pick_some(AllUncoveredCAs, UncoveredCAs).

pick_covered_cas(State, EffectorCAs, CoveredCAs) :-
	get_state(State, level, Level), 
	get_state(State, wards, Wards), UmweltLevel is Level - 1, 
	level_cas(UmweltLevel, CAs), 
	findall(CA, 
		(
			member(CA, CAs), 
			 \+ member(CA, EffectorCAs), 
			covered_by_any(CA, Wards)), AllCoveredCAs), 
	pick_some(AllCoveredCAs, CoveredCAs).

level_cas(Level, CAs) :-
	meta_ca : name_from_level(Level, MetaCA), 
	send_query(MetaCA, wards, CAs).

type_of(Name, Type) :-
	atomic_list_concat([Type|_], ':', Name).