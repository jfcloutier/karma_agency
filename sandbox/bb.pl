:- module(bb, []).

:- dynamic event/3.

add_event(Topic, Payload, Source) :-
    assertz(event(Topic, Payload, Source)).

remove_event(Topic,Payload, Source) :-
    retract(event(Topic, Payload, Source)).

%%% Code using this module as a blackboard
% run() :-
%     writeln("Waiting for events..."),
%     thread_wait(
%         handle_events(Events), 
%         [retry_every(30), module(bb),  wait_preds([+(event/3)]), modified(Events)]),
%     run().

% handle_events([]) :-
%     writeln("No events"), !, fail.

% handle_events(Events) :- 
%     format("Handling events ~w~n", [Events]),
%     foreach(bb:event(Topic, Payload, Source), handle_event(Topic,Payload,Source)).