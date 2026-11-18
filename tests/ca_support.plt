/*
Tests of CA support.

run_tests(ca_support).
*/

:- begin_tests(ca_support, []).

:- use_module(agency(som/ca_support)).
:- use_module(utils(logger)).

:- set_log_level(info).

test(grossly_more) :-
    assertion(\+ is_grossly_more_than(2, 3)),
    assertion(is_grossly_more_than(3, 0)),
    assertion(is_grossly_more_than(10, -10)),
    assertion(\+ is_grossly_more_than(16, 12)),
    assertion(is_grossly_more_than(16, 11)).

test(goal_id) :-
    Goal = goal{impact:persist,intent_level:1,priority:5.0,
                target:target{origin:object{id:'5b0bf6b7bf2e4d2bb92b3cfea2b3a69b441d87a7',type:synthetic, evidence:[aea569bd7fdfc659f6a0037e1df73a161c23f92a,c26e19952cc63e5d078ed7ebee1aba15341719e7]},
                              kind:count,
                              value:2}},
    goal_id(Goal, GoalId),
    log(info, ca_support, "Goal id is ~w", [GoalId]),
    atom(GoalId).

test(same_goal_ids) :-
    Goal1 = goal{impact:persist,intent_level:1,priority:5.0,
                target:target{origin:object{id:'5b0bf6b7bf2e4d2bb92b3cfea2b3a69b441d87a7',type:synthetic, evidence:[aea569bd7fdfc659f6a0037e1df73a161c23f92a,c26e19952cc63e5d078ed7ebee1aba15341719e7]},
                              kind:count,
                              value:2}},
    Goal2 = goal{impact:persist,intent_level:1,priority:1.0,
                target:target{origin:object{id:'5b0bf6b7bf2e4d2bb92b3cfea2b3a69b441d87a7',type:synthetic, evidence:[aea569bd7fdfc659f6a0037e1df73a161c23f92a,c26e19952cc63e5d078ed7ebee1aba15341719e7]},
                              kind:count,
                              value:2}},

    goal_id(Goal1, GoalId1),
    goal_id(Goal2, GoalId2),
    assertion(GoalId1 == GoalId2).

test(different_goal_ids) :-
    Goal1 = goal{impact:persist,intent_level:1,priority:5.0,
                target:target{origin:object{id:'5b0bf6b7bf2e4d2bb92b3cfea2b3a69b441d87a7',type:synthetic, evidence:[aea569bd7fdfc659f6a0037e1df73a161c23f92a,c26e19952cc63e5d078ed7ebee1aba15341719e7]},
                              kind:count,
                              value:2}},
    Goal2 = goal{impact:persist,intent_level:1,priority:5.0,
                target:target{origin:object{id:'5b0bf6b7bf2e4d2bb92b3cfea2b3a69b441d87a7',type:synthetic, evidence:[aea569bd7fdfc659f6a0037e1df73a161c23f92a,c26e19952cc63e5d078ed7ebee1aba15341719e7]},
                              kind:count,
                              value:3}},

    goal_id(Goal1, GoalId1),
    goal_id(Goal2, GoalId2),
    assertion(GoalId1 \= GoalId2).
