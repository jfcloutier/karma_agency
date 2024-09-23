/*
Cognition actor (CA)

A CA strives to become increasingly compentent at making sense of its umwelt and at altering its own beliefs.

A CA makes sense of its umwelt by predicting observations (i.e. beliefs of umwelt CAs)
and by deriving beliefs from past observations, beliefs that then become observations available to parent CAs (higher-level CAs that have the CA in their umwelts).

A CA is effective at impacting its umwelt if it has reliable policies it can execute to validate pleasant beliefs or invalidate unpleasant beliefs.

The more competent a CA is, the more likely it is that its parents CAs will be competent as well and survive,
and thus the more likely it is that the CA itself will survive since orphaned CAs are susceptible to being removed.

Event: 
* In
  * fullness, [level(0..1)]
  * integrity, [level{0..1)]
  * engagement, [level(0..1)]

* Out
  * ca_started, [level(Level)]
  * ca_terminated, [level(Level)]

* Queries
  * type - ca
  * level - Integer > 0
  * umwelt - list of CA names
    
State:
    * level - 1..? - the level of the CAs it is responsible for
    * timer - the frame timer
    * umwelt - ward CAs, or 'unknown' if they need to be discovered from querying the SOM
    * frame - Integer
    * history - [Frame, ...]

*/

:- module(ca, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(pubsub)).
:- use_module(library(uuid)).

name_from_level(Level, Name) :-
    uuid(ID),
    atomic_list_concat([ca, level, Level, id, ID], ":", Name).

% A meta-cognition actor re-evaluates the  state of its layer of the SOM every 2 ** Level seconds
latency(Level, Latency) :-
    Latency is (2 ** Level) * 100.

level(Name, Level) :-
    send_query(Name, level, Level).

umwelt(Name, Umwelt) :-
    send_query(Name, umwelt, Umwelt).

% Worker

init(Options, State) :-
    log(info, ca, "Initiating ca with ~p", [Options]),
    empty_state(EmptyState),
    option(level(Level), Options),
    option(umwelt(Umwelt), Options),
    self(Name),
    latency(Level, Latency),
    send_at_interval(Name, framer, message(start_frame, Name), Latency, Timer),
    send_message(Name, start_frame),
    put_state(EmptyState, [level-Level, timer-Timer, umwelt-Umwelt, frame-0, history-[]], State),
    publish(ca_started, [level(Level)]).

terminate :-
    log(warn, ca, 'Terminated').

handle(terminating, State) :-
    log(warn, ca, '~@ terminating', [self]),
    get_state(State, timer, Timer),
    get_state(State, level, Level),
    timer:stop(Timer),
    publish(ca_terminated, [level(Level)]).

% End current frame, if any, and start new frame
handle(message(start_frame, _), State, NewState) :-
    end_frame(State, State1),
    start_frame(State1, NewState).

handle(message(Message, Source), State, State) :-
   log(info, ca, '~@ is NOT handling message ~p from ~w', [self, Message, Source]).

handle(query(type), _, ca).

handle(query(level), State, Level) :-
    get_state(State, level, Level).

handle(query(umwelt), State, Umwelt) :-
    get_state(State, umwelt, Umwelt).


handle(query(Query), _, unknown) :-
    log(info, ca, '~@ is NOT handling query ~p', [self, Query]).

% TODO
end_frame(State, State) :-
    log(info, ca, 'CA ~@ is ending the current frame', [self]).

% TODO
start_frame(State, State) :-
    log(info, ca, 'CA ~@ is starting a new frame', [self]).