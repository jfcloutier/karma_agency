%% Start/stop shortcuts and callbacks for an worker thread named alice.

:- module(alice, []).

:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(worker)).
:- use_module(actor_model(pubsub), [publish/2]).

%% Worker callbacks

init(Options, State) :-
   option(mood(Mood), Options, bored),
    format("[alice] Initializing with mood ~w~n", [Mood]),
    empty_state(EmptyState),
    put_state(EmptyState, mood, Mood, State).

terminate :-
   writeln("[alice] Terminating").

% Ignore all events from self
handle(event(_,_, alice)).

handle(event(party, PartyGoers, _), State, NewState) :-
   format("[alice] Ready to party with ~w~n", [PartyGoers]),
   put_state(State, mood, pleased, NewState),
   contact_others(PartyGoers),
   publish(police, "Anything wrong, officer?").

handle(event(police, Payload, _), State, NewState) :-
   format("[alice] Police! ~w~n", [Payload]),
   put_state(State, mood, displeased, NewState).

handle(query(mood), State, Response) :-
   get_state(State, mood, Response),
   format("Alice is ~p~n", [Response]).

handle(query(_), _, "Pardon?").

handle(message(Message, Source), State, State) :-
    format("[alice] Received ~w from ~w~n", [Message, Source]).
 
%% Private

contact_others([]).
contact_others([alice | Others]) :-
   contact_others(Others).
contact_others([Name | Others]) :-
   worker:send(Name, "Alice says howdy!"),
   contact_others(Others).

