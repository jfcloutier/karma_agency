%% Start/stop shortcuts and callbacks for an actor thread named alice.

:- module(alice, [start_alice/1, stop_alice/0]).

:- use_module(actor).
:- use_module(pubsub, [publish/2]).

%% Public

start_alice(Supervisor) :-
   supervisor:start_child(
      Supervisor,
      actor, 
      alice, 
      [topics([party, police]), 
      init(alice:init), 
      terminate(alice:terminate), 
      handler(alice:handle)]
      ).

stop_alice() :-
   actor:stop(alice).

%% Callbacks

init() :-
    writeln("[alice] Initializing").

terminate() :-
   writeln("[alice] Terminating").

% Ignore all events from self
handle(event(_,_, alice)).

handle(event(party, PartyGoers, _)) :-
   format("[alice] Ready to party with ~w~n", [PartyGoers]),
   contact(PartyGoers),
   publish(police, "Anything wrong, officer?").

handle(event(police, Payload, _)) :-
   format("[alice] Police! ~w~n", [Payload]).

handle(message(Message, Source)) :-
    format("[alice] Received ~w from ~w~n", [Message, Source]).
 
%% Private

contact([]).
contact([alice | Others]) :-
   contact(Others).
contact([Name | Others]) :-
   actor:send(Name, "Alice says howdy!"),
   contact(Others).

