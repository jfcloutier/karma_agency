/*

Meta-cognition actor (meta-CA)

A meta-CA adds and removes cognition actors (CAs) at the level of the SOM it manages.

Events:

* In
  * fullness, [level(0..1)]
  * integrity, [level{0..1)]
  * engagement, [level(0..1)]

* Out
  * mca_started, [level(Level)]

* Messages
  * TBD
    
State:
    * level - 1..? - the level of the CAs it is responsible for
    * frame - 1..? - the current frame, incremented from 0
    * timer - the frame timer
    * history - latest frames

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

%% Worker

init(Options, State) :-
    log(info, meta_ca, "Initiating with ~p", [Options]),
    empty_state(EmptyState),
    option(level(Level), Options),
    subscribe(ca_started),
    self(Name),
    latency(Level, Latency),
    send_at_interval(Name, curator, event(curate, true, Name), Latency, Timer),
    put_state(EmptyState, [level-Level, timer-Timer, frame-0, history-[]], State),
    publish(mca_started, [level(Level)]).

terminate :-
    log(warn, meta_ca, 'Terminated').

handle(message(Message, Source), State, State) :-
   log(info, meta_ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(event(ca_started, Payload, Source), State, NewState) :-
    options(level(Level), Payload),
    % A ward of the meta CA
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
curate(State, State) :-
    get_state(State, level, Level),
    log(debug, meta_ca, '~@ assessing level ~w', [self, Level]),
    cull(State),
    grow(State).

% For each ward that is not dependent on by a CA at a higher level,
% evaluate if it is persistently useless.
% If so, terminate it.
cull(_) :-
    self(Name),
    log(debug, meta_ca, '~w is culling ', [Name]).

% Add a CA to the metaCA's level L if leve L - 1 is complete and level L is not covering it.
grow(State) :-
    self(Name),
    get_state(State, level, Level),
    UmweltLevel is Level - 1,
    is_level_complete(UmweltLevel),
    \+ is_level_covering(Level),
    add_ca(Level, NewCA),
    log(debug, meta_ca, '~w added CA ~p', [Name, NewCA]).
grow(_).

% TODO
is_level_complete(0).

% TODO
is_level_covering(_) :- fail.

% TODO
add_ca(_,_) :- fail.
 

