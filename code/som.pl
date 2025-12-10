/*
The Society of Mind (SOM) of an agent is an evolving collective of Cognition Actors
forming a hierarchy.
*/

:- module(som, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(pubsub)).
:- use_module(actors(supervisor)).
:- use_module(agency(som/sensor_ca)).
:- use_module(agency(som/effector_ca)).
:- use_module(agency(som/dynamic_ca )).

%! initialized(+Sensors, +Effectors, +BodyHost, +Options) is det
% the SOM is initiated
initialized(Sensors, Effectors, BodyHost, _) :-
	log(info, som, "Initializing the SOM with static CAs"),
	Children = [
		% Will start the actors' pubsub
		pubsub,
		% Will start the SOM supervisor with no children yet
		supervisor(som,
			[restarted(permanent)])],
	% Start agency - the top supervisor
	supervisor : started(agency,
		[children(Children), body_host(BodyHost)]),
	sensor_cas_started(Sensors),
	effector_cas_started(Effectors).
	

% Starting sensor and effector CAs
sensor_cas_started([]).

sensor_cas_started([Sensor|Others]) :-
	sensor_ca : name_from_sensor(Sensor, Name),
	supervisor : worker_child_started(som,
		sensor_ca,
		Name,
		[
			init([sensor(Sensor)])]),
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

% Create the first CA. Give it all static CAs as its umwelt.
growing :-
	dynamic_ca : name_from_level(1, CA),
	log(info, som, "First level one CA ~p", [CA]),
    static_cas(Umwelt),
	supervisor : worker_child_started(som,
		dynamic_ca,
		CA,
		[init([level(1), umwelt(Umwelt)])]),
	log(info, som, "Added new CA ~w at level ~w with umwelt ~p", [CA, 1, Umwelt]).
	
% % Recruit 2 or more CAs at the given level eager to participate in the umwelt of a CA one level up
% umwelt_recruited(Level, Umwelt) :-
% 	recruits(Level, Recruits),
% 	pick_some(Recruits, Umwelt).

% recruits(Level, Recruits) :-
% 	findall(CA,
% 		at_level(Level, CA),
% 		Candidates),
% 	length(Candidates, N),
% 	N > 1,
% 	random_permutation(Candidates, PermutatedCandidates),
% 	findall(Eagerness - Recruit,
% 		(member(Recruit, PermutatedCandidates),
% 			recruit(Recruit, Eagerness),
% 			Eagerness > 0),
% 		Pairs),
% 	keysort(Pairs, SortedPairs),
% 	pairs_values(SortedPairs, Recruits).

% recruit(CA, P) :-
% 	ca_module_from_name(CA, Module),
% 	Module : recruit(CA, P).

at_level(Level, CA) :-
	supervisor : children(som, CAs),
	member(child(worker, CA), CAs),
	level_from_name(CA, Level).

level_from_name(CA, Level) :-
	ca_module_from_name(CA, Module),
	Module : level_from_name(CA, Level).

ca_module_from_name(CA, Module) :-
	atomic_list_concat([Type|_], ":", CA),
	type_module(Type, Module).

type_module(sensor, sensor_ca).
type_module(effector, effector_ca).
type_module(ca, dynamic_ca).

static_cas(StaticCAs) :-
	sensor_cas(SensorCAs),
	effector_cas(EffectorCAs),
	append(SensorCAs, EffectorCAs, StaticCAs).

sensor_cas(SensorCAs) :-
    query_answered(som, children, SOMChildren),
    findall(CA,
		(member(child(worker, CA),
			SOMChildren), query_answered(CA, type, sensor_ca)),
		SensorCAs).

effector_cas(EffectorCAs) :-
    query_answered(som, children, SOMChildren),
    findall(CA,
		(member(child(worker, CA),
			SOMChildren), query_answered(CA, type, effector_ca)),
		EffectorCAs).
