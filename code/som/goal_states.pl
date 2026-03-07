/*
Utilities for goal states

goal_state{goal: Goal, status: Status, received:Boolean messages: [GoalMessage, ...]}
message{about:About, from:Source}
goal{id: ID, target: Target, impact: Impact, priority: Priority, intent_id:IntentId, intent_level: Level}
target{origin: Origin, kind: Kind, value: Value}

Received is true in a goal state if this is a goal the CA is expected to realize. It is false if the goal is one the CA exepcts its umwelt to realize.

Goal satus:

* `none` - an undeclared  but potential goal - typically a goal from a sibling CA the CA gathers info about in case it later becomes a declared goal
* `todo` - no progress yet on todo goal
* `can_seek` - the goal was found to relate to one or more experiences of the CA
* `cannot_seek` - the goal does not relate to any experience
* `planning` - searching for a transitively feasible plan to achieve the goal
* `can_execute` - the goal has a feasible plan
* `cannot_execute` - no feasible plan can be found
* `executing` - transitively executing the plan to achieve the goal
* `executed` - the plan for the goal was executed "all the way down"
* `achieved` - the goal was achieved from executing a plan for it

*/

:- module(goal_states, [goal_progress/2, is_deadend_status/1, is_goal_step/1, goal_state_is_meaningful/1, goal_state_is_advancing/1, urgent_goal_state/2, goal_state_message_added/3]).

:- use_module(utils(logger)).

% Goal status steps progressing from none to achieved.
goal_progress(none, todo).
goal_progress(todo, can_seek).
goal_progress(can_seek, planning).
goal_progress(planning, can_execute).
goal_progress(can_execute, executing).
goal_progress(executing, executed).
goal_progress(executed, achieved).

% Goal step that dead ends progress (but only should the whole umwelt report it for a directive)
is_deadend_status(cannot_seek).
% Goal step that dead ends progress (but only should the all umwelt CAs who reported can_seek for a directive later report it for the directive)
is_deadend_status(cannot_execute).

is_goal_step(Step) :-
	goal_progress(Step, _) ; goal_progress(_, Step) ; is_deadend_status(Step).

% Is the status of a goal indicating that progress can be made?
goal_state_is_meaningful(GoalState) :-
    memberchk(GoalState.status, [can_seek, planning, can_execute, executing]).

% Is the status of a goal indicating that progress is being made?
goal_state_is_advancing(GoalState) :-
    memberchk(GoalState.status, [planning, can_execute, executing]).

% Non-deterministic
% Identify a goal state urgently in need of being advanced. 
% The urgency of a goal state is combination of the originating intent's level, the goal's priority and how close to being executed it is.
% The statuses in increasing order of priority:
%   none -> A goal is at status none when a CA first learns about a directive an umwelt CA received from another CA - priority is 0 - nothing to do
%   can_seek -> the goal might be put into planning by a parent - priority is 0 - nothing to do yet
%   todo -> find if can_seek or cannot_seek: send event out
%   planning -> build a plan: publish can_execute or cannot_execute if successful vs not
%   executing -> execute the next unexecuted directive in a plan for the goal: if all are executed, publish event executed
urgent_goal_state(GoalStates, GoalState) :-
     log(info, goal_states, "Choosing the urgent goal state within ~p", [GoalStates]),
     random_permutation(GoalStates, PermutedGoalStates),  
     map_list_to_pairs(urgency, PermutedGoalStates, Pairs),
     keysort(Pairs, SortedPairs),
     % Most urgent first
     reverse(SortedPairs, SortedPairs1),
     pairs_values(SortedPairs1, SortedGoalStates),
     member(GoalState, SortedGoalStates).

intent_level(GoalState, Level) :-
    Level = GoalState.goal.intent_level.

% The urgency of a goal state is the combination of
% - the level of the intent causing the goal (the higher, the more urgent)
% - the priority of the goal
% - how close it is to being executed
urgency(GoalState, 0) :-
    memberchk(GoalState.status, [none, can_seek, cannot_seek, cannot_execute, executed, achieved]), !.

urgency(GoalState, Urgency) :-
    log(info, goal_states, "Computing the urgency of ~p", [GoalState]),
    Level = GoalState.goal.intent_level,
    Priority = GoalState.goal.priority,
    advancement(GoalState.status, Advancement),
    Urgency is Level * Priority * Advancement.
    
advancement(Status, Advancement) :-
    nth1(Advancement, [planning, can_execute, executing], Status).

goal_state_message_added(GoalState, Message, UpdatedGoalState) :-
    memberchk(Message, GoalState.messages) ->
        UpdatedGoalState = GoalState
        ;
        UpdatedGoalState = GoalState.put(messages, [Message | GoalState.messages]).
