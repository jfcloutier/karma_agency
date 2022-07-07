%% Alice  is an actor

:- module(alice, [start_alice/0, stop_alice/0]).

:- use_module(actor).
:- use_module(supervisor, [supervise/3, stop_supervised/3]).
:- use_module(pubsub, [publish/2]).

start_alice() :-
   supervise(actor, alice, [topics([party, police]), init(alice:init), handler(alice:handle)]).

stop_alice() :-
   stop_supervised(actor, alice, [termination(alice:terminate)]).

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
 
contact([]).
contact([alice | Others]) :-
   contact(Others).
contact([Name | Others]) :-
   actor:send(Name, "Alice says howdy!"),
   contact(Others).

