%% Start/stop shortcuts and callbacks for an actor thread named bob.

:- module(bob, [start_bob/1, stop_bob/0]).

:- use_module(actor_model(actor)).
:- use_module(actor_model(pubsub), [publish/2]).

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
