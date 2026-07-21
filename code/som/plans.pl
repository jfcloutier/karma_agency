/*
Utilities for plans

plan{goal: goal{...}, directives: [goal{...} | command{...}, ...]}
*/

:- module(plans, [is_plan_active/2, has_plan/3, same_plans/2]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/goals)).
:- use_module(agency(som/ca_support)).

% A plan is active if the directives are goals (not commands) and not all directives are observed as executed, not_relevant or failed
is_plan_active(Plan, State) :-
	forall(member(Directive, Plan.directives), (is_goal(Directive), \+ is_directive_inactive(Directive, State))).

% A directive is inactive if it is observed as executed, not_relevant or failed
is_directive_inactive(Goal, State) :-
	get_state(State, observations, Observations),
	member(Observation, Observations),
	is_activation_observation(Observation, Goal),
	member(Observation.value, [not_relevant, executed, failed]).

% An observation is of the activation of a goal
is_activation_observation(Observation, Goal) :-
	observation{origin:Object, kind:activation} :< Observation,
	object{type:goal, id:GoalId} :< Object,
	goal_id(Goal, GoalId).

% There is a plan for the goal
has_plan(Goal, State, Plan) :-
	get_state(State, plans, Plans),
	member(Plan, Plans),
	plan{goal:Goal} :< Plan.

% Two plans are the same if the goals have identical targets and impact (same goal id)
% and the same list of directives (order matters)
same_plans(Plan, OtherPlan) :-
	same_goals(Plan.goal, OtherPlan.goal),
	same_directives(Plan.directives, OtherPlan.directives).

same_directives([], []).
same_directives([Directive | Rest], [OtherDirective | OtherRest]) :-
	same_goals(Directive, OtherDirective),
	same_directives(Rest, OtherRest).

	