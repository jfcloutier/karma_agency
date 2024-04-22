/*
[load].
['tests/apperception/apperceive.plt'].
run_tests(apperceive).
*/

:- begin_tests(apperceive).

:- use_module(code(logger)).
:- use_module(apperception(sequence)).
:- use_module(apperception(apperception_engine)).
:- use_module(tests(apperception/leds_observations)).
:- use_module(tests(apperception/eca_observations)).

test(find_led_causal_theories) :-
    set_log_level(note),
    sequence(leds_observations, Sequence), 
    MaxSignatureExtension = max_extension{max_object_types:2, max_objects:2, max_predicate_types:2},
    ApperceptionLimits = apperception_limits{max_signature_extension: MaxSignatureExtension, max_theories_per_template: 1000, good_enough_coverage: 70, keep_n_theories: 3, funnel: 3-2, time_secs: 30},
    apperceive(Sequence, ApperceptionLimits, Theories),
    length(Theories, Length),
    assertion(Length > 0).

test(find_eca_causal_theories) :-
    set_log_level(note),
    sequence(eca_observations, Sequence), 
    MaxSignatureExtension = max_extension{max_object_types:0, max_objects:0, max_predicate_types:0},
    ApperceptionLimits = apperception_limits{max_signature_extension: MaxSignatureExtension, good_enough_coverage: 85, keep_n_theories: 3, funnel: 10-5, time_secs: 30},
    apperceive(Sequence, ApperceptionLimits, Theories),
    length(Theories, Length),
    assertion(Length > 0).



:- end_tests(apperceive).

