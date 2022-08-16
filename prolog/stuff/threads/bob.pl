%% Start/stop shortcuts and callbacks for an actor thread named bob.

:- module(bob, [start_bob/1, stop_bob/0]).

:- use_module(actor).
:- use_module(pubsub, [publish/2]).

%% Public

start_bob(Supervisor) :-
   supervisor:start_child(
      Supervisor,
      actor, 
      bob, 
      [topics([party, police]), 
      init(bob:init), 
      handler(bob:handle), 
      terminate(bob:terminate), 
      restart(permanent)]
      ).

stop_bob() :-
   actor:stop(bob).

%% Callbacks

init() :-
    writeln("[bob] Initializing").

terminate() :-
   writeln("[bob] Terminating").

% Ignore all events from self
handle(event(_,_, bob)).

handle(event(party, PartyGoers, _)) :-
   format("[bob] Ready to party with ~w~n", [PartyGoers]),
   contact(PartyGoers),
   publish(police, "Wassup?").

handle(event(police, Payload, _)) :-
   format("[bob] Police! ~w~n", [Payload]).
   % writeln("[bob] Panic!"),
   % throw(exit(panic)).

handle(message(Message, Source)) :-
   format("[bob] Received ~w from ~w~n", [Message, Source]).

%% Private

contact([]).
contact([bob | Others]) :-
   contact(Others).
contact([Name | Others]) :-
   actor:send(Name, "Bob says howdy!"),
   contact(Others).

%% cd('prolog/stuff/threads').
%% [thread_utils, supervisor, pubsub, actor, bob, alice].

test :-
   supervisor:start(top),
   supervisor:start_child(top, pubsub, [restart(transient)]),
   start_bob(top),
   start_alice(top),
   pubsub:publish(party, [alice, bob]),
   sleep(1),
   stop_bob,
   supervisor:kill_child(top, actor, bob),
   stop_alice,
   supervisor:kill_child(top, pubsub), 
   supervisor:stop(top),
   sleep(1),
   threads.

test1 :-
   supervisor:start(top),
   supervisor:start_child(top, pubsub, [restart(transient)]),
   supervisor:start_child(top, supervisor, bottom, [restart(transient)]),
   start_bob(bottom),
   start_alice(bottom),
   pubsub:publish(party, [alice, bob]),
   sleep(1),
   supervisor:stop(top),
   sleep(1),
   threads.
