/*

The feeling of fear.

*/

:- module(competence, []).

:- use_module(code(logger)).
:- use_module(actor_model(actor_utils)).

name(competence).

init(_, State) :-
    log(info, competence, 'Initiating'),
    empty_state(State),
    send_message(start).

terminate :-
    log(info, competence, 'Terminating').
    
handle(message(Message, Source), State, State) :-
   log(info, competence, 'Handling message ~p from ~w in state ~p', [Message, Source, State]).

handle(event(Topic, Payload, Source), State, State) :-
    log(info, competence, 'Handling event event(~w, ~p, ~w) in state ~p', [Topic, Payload, Source, State]).

handle(query(Query), State, tbd) :-
    log(info, competence, 'Handling query ~p in state ~p', [Query, State]).