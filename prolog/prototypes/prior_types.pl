:- module(prior_types, [prior_object_type/1, prior_value_type/2, prior_property_type/3]).

%% Static (prior) vocabulary

prior_object_type(surface).
prior_object_type(obstacle).
prior_object_type(food).
prior_object_type(agent).

prior_property_type(color_of, surface, color). % color_of(floor, white)
prior_property_type(color_of, obstacle, color). % color_of(obstacle_1, brown)
prior_property_type(distance_to, obstacle, centimeters).
prior_property_type(distance_to, food, centimeters).
prior_property_type(distance_to, agent, centimeters).
prior_property_type(direction_to, obstacle, degrees).
prior_property_type(direction_to, food, degrees).
prior_property_type(direction_to, agent, degrees).
prior_property_type(touching, obstacle, truth).
prior_property_type(brightness, surface, luminance).

prior_value_type(color, [white, black, brown, red, green, yellow, blue]).
prior_value_type(centimeters, range(0, 100)).
prior_value_type(degrees, range(-180, 180)).
prior_value_type(truth, [true, false]).
prior_value_type(luminance, range(0, 10)).







