:- module(gm_1, []).

% This is the file produced from the dynamic gm_1 module.

% Umwelt
in_object_domain(obstacle_1, obstacle).
in_object_domain(obstacle_2, obstacle).

% Sensory domain

in_sensory_domain(distance_to, obstacle).

% Belief domain

in_belief_domain(b_obstacle_1, obstacle, truth).

% Action domain

in_action_domain(action_1, effect_on(run_positive, left_motor)).
in_action_domain(action_2, effect_on(run_positive, right_motor)).

% has_predictor(predictor{coverage:0, variables:[], static_rules:[], causal_rules:[], integrity_rules:[]}).

