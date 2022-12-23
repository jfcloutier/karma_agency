:- module(causal_theory, [rated_causal_theory/3]).

:- use_module(type_signature).
:- use_module(templates).

% Given a sequence, build a causal theory for it (if there is still time) and rate it.
rated_causal_theory(Sequence, CausalTheory, Rating) :-
    min_type_signature(Sequence, TypeSignature),
    % generate
    theory_template(TypeSignature, limits(num_object_types(1), num_objects(1), num_predicate_types(2)), Template),
    time_limit(Template, TimeLimit),
    % generate
    causal_theory(Template, TimeLimit, CausalTheory),
    rating(CausalTheory, Sequence, Rating).

time_limit(template(_, theory_bounds(_, _, max_atoms(MaxAtoms))), TimeLimit) :-
    TimeLimit is MaxAtoms * 0.01.

causal_theory(template(ExtendedTypeSignature, TheoryComplexityBounds), TimeLimit, CausalTheory) :-




% cd('sandbox/prototypes/apperception').
% [leds_observations, sequence, type_signature, domains, templates, causal_theory].
% sequence(leds_observations, Sequence), rated_causal_theory(Sequence, CausalTheory, Rating)