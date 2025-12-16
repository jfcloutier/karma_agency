:- module(test_helper, [init_som/0, terminate_som/0, get_message/1, get_message/2, get_matching_message/2, get_matching_message/3, assert_wellbeing_changed/3]).

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

get_matching_message(Pattern, Term) :-
    get_matching_message(Pattern, Term, 5).

get_matching_message(Pattern, Term, Timeout) :-
    freeze_var_in_term(Pattern, Term),
    thread_self(Name),
    thread_get_message(Name, Term, [timeout(Timeout)]), !.

get_matching_message(Pattern, Term, _) :-
    log(info, test_helper, "Failed to get message ~p matching ~p", [Term, Pattern]),
    fail.

freeze_var_in_term(Pattern, Term) :-
    var_in_term(Var, Term),
    !,
    freeze_var(Var, Pattern).

var_in_term(Var, Term) :-
    var(Term),
    Var = Term.

var_in_term(Var, [Term | Rest]):-
    var_in_term(Var, Term) ; var_in_term(Var, Rest).

var_in_term(Var, Term) :-
    Term =.. [_ | Args],
    var_in_term(Var, Args).

freeze_var(Var, Pattern) :-
    is_dict(Pattern), !,
    freeze(Var, Pattern :< Var).

freeze_var(Var, Pattern) :-
    is_list(Pattern), !,
    freeze(Var, member(Var, Pattern)).

freeze_var(Var, Pattern) :-
    freeze(Var, unifiable(Var, Pattern, _)).

% assert_wellbeing_changed(PreviousState, NewState, [fullness = <, integrity = <, engagement = >])
% Compares new wellbeing metrics to previous ones
assert_wellbeing_changed(PreviousWellbeing, NewWellbeing, Changes) :-
    log(info, test_helper, "Comparing previous wellbeing ~p to new ~p with ~p", [PreviousWellbeing, NewWellbeing, Changes]),
    wellbeing{fullness:PreviousFullness, integrity:PreviousIntegrity, engagement:PreviousEngagement} :< PreviousWellbeing,
    wellbeing{fullness:NewFullness, integrity:NewIntegrity, engagement:NewEngagement} :< NewWellbeing,
    option(fullness(FullnessComp), Changes),
    option(integrity(IntegrityComp), Changes),
    option(engagement(EngagementComp), Changes),
    assertion(call(FullnessComp, NewFullness, PreviousFullness)),
    assertion(call(IntegrityComp, NewIntegrity, PreviousIntegrity)),
    assertion(call(EngagementComp, NewEngagement, PreviousEngagement)).
