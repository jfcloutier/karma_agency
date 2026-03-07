/*
Utilities for plans

plan{id: ID, goal_id: GoalID, directives: [goal{...} | Action, ...], status:Status, score: Score}

Status is unknown, possible, cannot_execute, can_execute, executing or executed
*/

:- module(plans, [plan/3, plan_for_directive/4, same_plans/2, plan_score/2]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(library(uuid)).

% Make an unscored plan with status yet unknown from a goal's id and a series of directives.
plan(GoalId, Directives, Plan) :-
	uuid(Id),
	Plan = plan{id: Id, goal_id: GoalId, directives: Directives, status: unknown, score: none}.

% Find a plan that can be executed
plan_for_directive(Directive, State, Status, Plan) :-
	get_state(State, plans, Plans),
	member(Plan, Plans),
    Plan.status == Status, 
	Plan.goal_id == Directive.id.

% Two plans are identical if they have equivalent directives in the same order
same_plans(Plan1, Plan2) :-
	same_directives(Plan1.directives, Plan2.directives).

same_directives([], []).
same_directives([Directive1 | Rest1], [Directive2 | Rest2]) :-
	% A goal's id is determined by its target and impact
	same_goal_or_action(Directive1, Directive2),
	same_directives([Rest1], [Rest2]).

same_goal_or_action(Item1, Item2) :-
	is_dict(Item1, goal),
	is_dict(Item2, goal),
	Item1.id == Item2.id.

same_goal_or_action(Item, Item) :-
	atom(Item).

plan_score(Plan, Score) :-
    Score = Plan.score.