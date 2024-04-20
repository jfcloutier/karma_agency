%% Start/stop shortcuts and callbacks for an worker thread named bob.

:- module(bob, [start_bob/1, stop_bob/0]).

:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(worker)).
:- use_module(actor_model(pubsub), [publish/2]).

%% Public

start_bob(Supervisor) :-
   supervisor:start_child(
      Supervisor,
      worker, 
      bob, 
      [topics([party, police]), 
      init(bob:init), 
      handler(bob:handle), 
      terminate(bob:terminate), 
      restart(permanent)]
      ).

stop_bob :-
   worker:stop(bob).

%% Callbacks

init(State) :-
    writeln("[bob] Initializing"),
    empty_state(EmptyState),
    put_state(EmptyState, mood, bored, State).

terminate :-
   writeln("[bob] Terminating").

% Ignore all events from self
handle(event(_,_, bob), State, State).

handle(event(party, PartyGoers, _), State, NewState) :-
   format("[bob] Ready to party with ~w~n", [PartyGoers]),
   put_state(State, mood, excited, NewState),
   contact_others(PartyGoers),
   publish(police, "Wassup?").

handle(event(police, Payload, _), State, NewState) :-
   format("[bob] Police! ~w~n", [Payload]),
   put_state(State, mood, panicking, NewState).

handle(query(mood), State, Response) :-
   get_state(State, mood, Response),
   format("Bob is ~p~n", [Response]).

handle(query(_), _, "Ugh?").

handle(message(Message, Source), State, State) :-
   format("[bob] Received ~w from ~w~n", [Message, Source]).

%% Private

contact_others([]).
contact_others([bob | Others]) :-
   contact_others(Others).
contact_others([Name | Others]) :-
   worker:send(Name, "Bob says howdy!"),
   contact_others(Others).
