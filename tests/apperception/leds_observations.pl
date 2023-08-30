:- module(leds_observations, []).

% sensed(What, About, When)
sensed(on, [object(led, a), false], 1).
sensed(on, [object(led, b), true], 1).
sensed(on, [object(led, a), true], 2).
sensed(on, [object(led, b), false], 2).
sensed(on, [object(led, a), true], 3).
sensed(on, [object(led, b), true], 3).
sensed(on, [object(led, b), true], 4).
sensed(on, [object(led, a), true], 5).
sensed(on, [object(led, b), false], 5).
sensed(on, [object(led, a), true], 6).
sensed(on, [object(led, b), true], 6).
sensed(on, [object(led, a), false], 7).
sensed(on, [object(led, b), true], 7).
sensed(on, [object(led, a), true], 8).
