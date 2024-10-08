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
  * mca_started, [level(Level)]
  * mca_terminated, [level(Level)]

* Queries
  * type - meta-ca
  * level - Integer > 0
  * wards - [CAName, ...]
    
State:
    * busy - true|false
    * wards - ward CAs, or 'unknown' if they need to be discovered from querying the SOM

*/

:-module(meta_ca, []).

:- [load].

:- use_module(actor_model(actor_utils)).
:- use_module(code(logger)).
:- use_module(actor_model(pubsub)).
:- use_module(actor_model(supervisor)).
:- use_module(actor_model(worker)).
:- use_module(som(ca)).

% Static state
:- thread_local level/1, timer/1.

name_from_level(Level, Name) :-
	atomic_list_concat([metaca, level, Level], ":", Name).

% A meta-cognition actor re-evaluates the state of its layer of the SOM every 2 ** Level seconds

latency(Level, Latency) :-
	Latency is (2**Level)*2.

%% Worker

init(Options, State) :-
	log(info, meta_ca, 'Initiating ~@ with ~p', [self, Options]), 
	empty_state(EmptyState), 
	option(
		level(Level), Options), 
	assert(
		level(Level)), 
	subscribe_all([ca_started, ca_terminated]), 
	latency(Level, Latency), 
	self(Name), 
	send_at_interval(Name, curator, 
		message(curate, Name), Latency, Timer), 
	assert(
		timer(Timer)), 
	put_state(EmptyState, [wards - unknown, busy - false], State), 
	publish(mca_started, 
		[level(Level)]).

process_signal(control(stop)) :-
	worker : stop.

terminate :-
	log(warn, meta_ca, '~@ terminating', [self]), 
	timer(Timer), 
	timer : stop(Timer), 
	level(Level), 
	publish(ca_terminated, 
		[level(Level)]), 
	log(warn, meta_ca, 'Terminated ~@', [self]).

handle(message(curate, _), State, NewState) :-
	get_state(State, busy, false)
	 ->
			curate(State, NewState);
	log(debug, meta_ca, 'Skipping curating because busy').

handle(message(busy, _), State, NewState) :-
	put_state(State, busy, true, NewState).

handle(message(Message, Source), State, State) :-
	log(info, meta_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(ca_started, Payload, CA), State, NewState) :-
	option(
		level(Level), Payload), 
	level(Level), !, 
	find_wards(State, Wards), 
	put_state(State, wards, [CA|Wards], NewState).

handle(event(ca_terminated, Payload, CA), State, NewState) :-
	option(
		level(Level), Payload), 
	level(Level), !, 
	find_wards(State, Wards), 
	delete(Wards, CA, RemainingWards), 
	put_state(State, wards, RemainingWards, NewState).

handle(event(Topic, Payload, Source), State, State) :-
	log(info, meta_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(name), _, Name) :-
	self(Name).

handle(query(type), _, meta_ca).

handle(query(level), State, Level) :-
	get_state(State, level, Level).

handle(query(wards), State, Wards) :-
	get_state(State, wards, Wards).

handle(query(Query), _, unknown) :-
	log(info, meta_ca, '~@ is NOT handling query ~p', [self, Query]).

% Curate by 
%   adding a ward CA to increase coverage (if needed)
%   and/or retiring a useless ward CA (if needed)

curate(State, NewState) :-
	send_message(busy), 
	level(Level), 
	log(debug, meta_ca, '~@ curating level ~w', [self, Level]), 
	cull(State), 
	grow(State), 
	put_state(State, busy, false, NewState).

% For each ward that is not dependent on by a CA at a higher level,
% evaluate if it is persistently useless.
% If so, terminate it.
% TODO
cull(_).

% Add a CA to the metaCA's level L if leve L - 1 is complete and level L is not.

grow(State) :-
	level(Level), 
	log(debug, meta_ca, '~@ is growing level ~w ', [self, Level]), UmweltLevel is Level - 1, 
	level_is_complete(UmweltLevel, State), 
	 \+ level_is_complete(Level, State), !, 
	log(info, meta_ca, 'Adding new ca at level ~w', [Level]), 
	add_ca(State, NewCA), 
	log(debug, meta_ca, '~@ added CA ~p', [self, NewCA]).
grow(_).

level_is_complete(0, _).

level_is_complete(Level, State) :-
	Level \= 0, UmweltLevel is Level - 1, 
	level_cas(UmweltLevel, UmweltCAs), 
	log(debug, meta_ca, 'Level ~w has umwelt CAs ~p', [Level, UmweltCAs]), 
	find_wards(State, WardCAs), 
	log(debug, meta_ca, 'Level ~w has wards CAs ~p', [Level, WardCAs]), 
	all_covered_by(UmweltCAs, WardCAs).


all_covered_by([], _).

all_covered_by([UmweltCA|Others], CAs) :-
	covered_by_any(UmweltCA, CAs), !, 
	all_covered_by(Others, CAs).

covered_by_any(UmweltCA, CAs) :-
	member(CA, CAs), 
	ca : umwelt(CA, Umwelt), 
	member(UmweltCA, Umwelt).

% If at level 1, always include all effector_ca's
% Then assemble an umwelt of at least 1 or more non-covered CA and 0 or more covered CAs from level L - 1
% Create a CA with that umwelt at level L

add_ca(State, Name) :-
	level(Level), 
	ca : name_from_level(Level, Name), 
	pick_umwelt(State, Umwelt), 
	supervisor : start_worker_child(som, ca, Name, 
		[init(
			[level(Level), 
			umwelt(Umwelt)])]), 
	log(info, meta_ca, 'Added new CA ~w at level ~w with umwelt ~p', [Name, Level, Umwelt]).

pick_umwelt(State, Umwelt) :-
	level(Level), 
	effector_cas(Level, EffectorCAs), 
	pick_uncovered_cas(State, UncoveredCAs), 
	pick_covered_cas(State, CoveredCAs), 
	flatten([EffectorCAs, UncoveredCAs, CoveredCAs], Umwelt).

effector_cas(1, EffectorCAs) :-
	supervisor : children(som, Children), 
	findall(Name, 
		(
			member(
				child(worker, Name), Children), 
			type_of(Name, effector)), EffectorCAs).

effector_cas(_, []).

pick_uncovered_cas(State, UncoveredCAs) :-
	level(Level), 
	get_state(State, wards, Wards), UmweltLevel is Level - 1, 
	level_cas(UmweltLevel, CAs), 
	findall(CA, 
		(
			member(CA, CAs), 
			 \+ covered_by_any(CA, Wards)), AllUncoveredCAs), 
	pick_some(AllUncoveredCAs, UncoveredCAs).

pick_covered_cas(State, CoveredCAs) :-
	level(Level), 
	get_state(State, wards, Wards), UmweltLevel is Level - 1, 
	level_cas(UmweltLevel, CAs), 
	findall(CA, 
		(
			member(CA, CAs), 
			covered_by_any(CA, Wards)), AllUncoveredCAs), 
	pick_some(AllUncoveredCAs, CoveredCAs).

find_wards(State, Wards) :-
	get_state(State, wards, unknown), !, 
	level(Level), 
	level_cas(Level, Wards).

find_wards(State, Wards) :-
	get_state(State, wards, Wards).

level_cas(Level, CAs) :-
	supervisor : children(som, Children), 
	findall(Name, 
		(
			member(
				child(worker, Name), Children), 
			type_of(Name, ca), 
			level_of(Name, Level)), CAs).

type_of(Name, ca) :-
	atomic_list_concat([Header|_], ':', Name), 
	member(Header, [sensor, effector, ca]), !.

type_of(Name, metaca) :-
	atomic_list_concat([metaca|_], ':', Name), !.

type_of(Name, Type) :-
	atomic_list_concat([Type|_], ':', Name).

level_of(Name, 0) :-
	atomic_list_concat([Header|_], ':', Name), 
	member(Header, [sensor, effector]), !.

level_of(Name, Level) :-
	atomic_list_concat([_, level, L|_], ':', Name), !, L == Level.