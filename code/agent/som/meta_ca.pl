/*
A meta-cognition actor adds and removes CAs at its level.

Constraints:

* Only competent (good predictor) CAs from level L - 1 can be in the (static) umwelt of a new CA at level L
* Level L is grown one CA at a time until all competent Level - 1 CAs are covered
* When creating a level L CA,
  * a meta-CA selects a random N >= 2 subset of all competent L-1 CAs as the new CA's umwelt such that 
    * it increases coverage of Level - 1
    * all effector CAs are transitively in the umwelt.
* A meta-CA can not cull a relevant CA at its level L, i.e. if the CA is in the umwelt of a more abstract CA at level L + 1.
* A meta-CA culls a non-relevant level L CA if it is persistently incompetent.

Lifecycle:

* A level L + 1 meta-CA is started by the Level L meta-CA when Level L completely covers Level L - 1
* A meta-CA stops when there are no CAs at level L - 1

Homeostasis:

* The response to critically low fitness events is to message a proposal to cull one non-relevant (but possibly competent) ward CA
* The fitness actor responsible for the event collects messaged proposals and the selects then best one
  * The fitness actor sends a message back OKing the culling

* If fullness is critically low, the meta-CA proposes to sacrifice the costliest of its non-relevant ward CAs
  * Cost = local cost + shared cost of its transitive umwelt
    * if an L - 1 CA is in the umwelt of a level L CA and no others, then that CA inherits the entire (unshared) cost of the L - 1 CA
* If integrity is critically low, the meta-CA proposes to sacrifice the non-relevant ward CA most directly responsible for loss of integrity
* If engegement is critically low, the meta-CA proposes to sacrifice the least active, non-relevant ward CA

Events:

* In
  * fullness, [level: 0..1]
  * integrity, [level: 0..1]
  * engagement, [level: 0..1]

* Out
  * ca_retired, [name: Name]
  * ca_added, [name: Name]

* Messages
  * to appropriate fitness actor
    * proposed_culling([ca: Name, level: Level, savings: Number]), 
  * from a fitness actor
    * cull([ca: WardName])
    
State:
    * level - 1..? - the level of the CAs it is responsible for
    * frame - 1..? - the current frame, incremented from 0 
    * wards - names all same-level CAs
    * timer - the frame timer

*/

:-module(meta_ca, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).

name_from_level(Level, Name) :-
    atomic_list_concat([metaca, Level], "_", Name).

% A meta-cognition actor re-evaluates the  state of its layer of the SOM every 2 ** Level seconds
latency(Level, Latency) :-
    Latency is 2 ** Level.

init(Options, State) :-
    log(info, meta_ca, "Initiating with ~p", [Options]),
    empty_state(EmptyState),
    option(level(Level), Options),
    subscribe(ca_started),
    self(Name),
    latency(Level, Latency),
    send_at_interval(Name, curator, event(curate, true, Name), Latency, Timer),
    put_state(EmptyState, [level-Level, wards-[], timer-Timer, frame-0], State),
    publish(mca_started, [level(Level)]).

terminate :-
    log(warn, meta_ca, 'Terminated').

handle(message(Message, Source), State, State) :-
   log(info, meta_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(ca_started, _, Source), State, NewState) :-
    send_query(Source, level, Level),
    get_state(State, level, Level),
    get_state(State, wards, Wards),
    put_state(State, wards, [Source | Wards], NewState).

handle(event(curate, _, _), State, NewState) :-
    curate(State, NewState).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, meta_ca, '~@ is NOT handling event ~w, with payload ~p from ~w)', [self, Topic, Payload, Source]).

handle(query(name), _, Name) :-
    self(Name).

handle(query(level), State, Level) :-
    get_state(State, level, Level).

handle(query(Query), _, unknown) :-
    log(info, meta_ca, '~@ is NOT handling query ~p', [self, Query]).

handle(terminating, State) :-
    get_state(State, timer, Timer),
    timer:stop(Timer).

% Curate by 
%   adding a ward CA to increase coverage (if needed)
%   retiring a useless ward CA (if needed)
curate(State, NewState) :-
    get_state(State, level, Level),
    log(info, meta_ca, '~@ assessing level ~w', [self, Level]),
    grow(State, State1),
    cull(State1, NewState).

% Find the number of inclusions in the umwelt of a ward of each level-1 sensor CAs, 
% Randomly choose one with the fewest number N of umwelt inclusions of a level-1 sensor CA
% If the frame is a multiple of N + 1,
% assemble an umwelt with the level-1 sensor CA, subject to constraints and create a level-0 CA with it.
grow(State, State).

% For each ward that is not dependent on by a CA at a higher level,
% evaluate if it is persistently useless.
% If so, terminate it.
cull(State, State).
