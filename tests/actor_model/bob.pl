%% Start/stop shortcuts and callbacks for an worker thread named bob.

:- module(bob, []).

:- use_module(actor_model(actor_utils)).
:- use_module(actor_model(worker)).
:- use_module(actor_model(timer)).
:- use_module(actor_model(pubsub), [publish/2]).

%% Worker callbacks

name(bob).

init(Options, State) :-
    option(mood(Mood), Options, bored),
    format("[bob] Initializing with mood ~w~n", [Mood]),
    empty_state(EmptyState),
    self(Name),
    send_at_interval(Name, clock, event(tictoc, true, Name), 1, Timer),
    put_state(EmptyState, [mood-Mood, timer-Timer], State).

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

handle(event(tictoc, _, _), State, State) :-
   format("[bob] Tictoc!").

handle(query(mood), State, Answer) :-
   get_state(State, mood, Answer),
   format("Bob is ~p~n", [Answer]).

handle(query(_), _, "Ugh?").

handle(message(Message, Source), State, State) :-
   format("[bob] Received ~w from ~w~n", [Message, Source]).

handle(terminating, State) :-
    get_state(State, timer, Timer),
    timer:stop(Timer).

%% Private

contact_others([]).
contact_others([bob | Others]) :-
   contact_others(Others).
contact_others([Name | Others]) :-
   worker:send_message(Name, "Bob says howdy!"),
   contact_others(Others).
