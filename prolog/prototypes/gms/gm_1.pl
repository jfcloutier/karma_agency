:-module(gm_1, []).

% Umwelt
in_umwelt(obstacle_1, obstacle).
in_umwelt(obstacle_2, obstacle).

% Sensory domain

in_sensory_domain(distance_to, obstacle).

% Belief domain

in_belief_domain(b_obstacle_1, obstacle, truth).

% Action domain

in_action_domain(action_1, effect_on(run_positive, left_motor)).
in_action_domain(action_2, effect_on(run_positive, right_motor)).

