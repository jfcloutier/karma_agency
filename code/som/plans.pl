/*
Utilities for plans

plan{goal: goal{...}, directives: [goal{...} | command{...}, ...]}
*/

:- module(plans, [is_plan_active/2]).

:- use_module(utils(logger)).
:- use_module(actors(actor_utils)).
:- use_module(agency(som/goals)).

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

	