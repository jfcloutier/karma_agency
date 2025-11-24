:- module(test_helper, [init_som/0, terminate_som/0, get_message/1, get_message/2, assert_wellbeing_changed/3]).

:- use_module(actors(actor_utils)).
:- use_module(actors(supervisor)).
:- use_module(agency(agent)).
:- use_module(utils(logger)).

init_som :-
	agent : started('localhost:4000', []).

terminate_som :-
	supervisor : stopped(agency, 60).

get_message(Term) :-
    get_message(Term, 5).

get_message(Term, Timeout) :-
    thread_self(Name),
    thread_get_message(Name, Term, [timeout(Timeout)]).

% assert_wellbeing_changed(PreviousState, NewState, [fullness = <, integrity = <, engagement = >])
% Compares new wellbeing metrics to previous ones
assert_wellbeing_changed(PreviousWellbeing, NewWellbeing, Changes) :-
    log(info, test_helper, "Comparing previous wellbeing ~p to new ~p with ~p", [PreviousWellbeing, NewWellbeing, Changes]),
    option(fullness(PreviousFullness), PreviousWellbeing),
    option(integrity(PreviousIntegrity), PreviousWellbeing),
    option(engagement(PreviousEngagement), PreviousWellbeing),
    option(fullness(NewFullness), NewWellbeing),
    option(integrity(NewIntegrity), NewWellbeing),
    option(engagement(NewEngagement), NewWellbeing),
    option(fullness(FullnessComp), Changes),
    option(integrity(IntegrityComp), Changes),
    option(engagement(EngagementComp), Changes),
    assertion(call(FullnessComp, NewFullness, PreviousFullness)),
    assertion(call(IntegrityComp, NewIntegrity, PreviousIntegrity)),
    assertion(call(EngagementComp, NewEngagement, PreviousEngagement)).
