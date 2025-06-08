/*
The Society of Mind (SOM) of an agent is an evolving collective of Cognition Actors
forming a hierarchy.
*/

:- module(som, []).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(actors(supervisor)).
:- use_module(som(sensor_ca)).
:- use_module(som(effector_ca)).
:- use_module(som(ca)).

%! started(+Sensors, +Effectors) is det
% the SOM is initiated
started(Sensors, Effectors) :-
	log(info, som, "Starting the SOM"),
	Children = [
		% Will start the actors' pubsub
		pubsub,
		% Will start the SOM supervisor with no children yet
		supervisor(som,
			[restarted(permanent)])],
	% Start agency - the top supervisor
	supervisor : started(agency,
		[children(Children)]),
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

% Create the first CA capable of mitosis. Give it an umwelt
level_one_ca_started :-
	ca : name_from_level(1, CA),
	umwelt_recruited(0, Umwelt),
	supervisor : worker_child_started(som,
		ca,
		CA,
		[
			init([level(1), umwelt(Umwelt)])]),
	forall(member(UmweltCA, Umwelt), message_sent(UmweltCA, adopted)),
	log(info, som, "Added new CA ~w at level ~w with umwelt ~p", [CA, 1, Umwelt]).
	
% Recruit at the given level 2 or more CAs most eager CAs to participate in the umwelt of a CA one level up
umwelt_recruited(Level, Umwelt) :-
	recruits(Level, Recruits),
	pick_some(Recruits, Umwelt).

recruits(Level, Recruits) :-
	findall(CA,
		at_level(Level, CA),
		Candidates),
	length(Candidates, N),
	N > 1,
	random_permutation(Candidates, PermutatedCandidates),
	findall(Eagerness - Recruit,
		(member(Recruit, PermutatedCandidates),
			recruit(Recruit, Eagerness),
			Eagerness > 0),
		Pairs),
	keysort(Pairs, SortedPairs),
	pairs_values(SortedPairs, Recruits).

recruit(CA, P) :-
	ca_module_from_name(CA, Module),
	Module : recruit(CA, P).

at_level(Level, CA) :-
	supervisor : children(som, CAs),
	member(CA, CAs),
	level_from_name(CA, Level).

level_from_name(CA, Level) :-
	ca_module_from_name(CA, Module),
	Module : level_from_name(CA, Level).

ca_module_from_name(CA, Module) :-
	atomic_list_concat([Module|_], ":", CA).