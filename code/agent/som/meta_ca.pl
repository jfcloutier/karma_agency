/*

Meta-cognition actor (meta-CA)

A meta-CA adds and removes cognition actors (CAs) at the level of the SOM it manages.

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
