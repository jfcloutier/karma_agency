# How to discover a better predictor

Given a Partial sensory sequence/action memory sequence (instantiated by dropping states from a full Sequence),

Find, if applicable and within a set time limit, a valid Predictor with greater Performance than any previously found.

Partial Sequence: Ordered subset of the states in a memory sequence.

Applicable: If the Partial Sequence is not empty.

Performance: K1 x Accuracy + K2 x Complexity

Accuracy: The fractional length of the Partial Sequence (what the predictor covers)

Complexity: The number of atoms in Static and Causal Rules

Valid: If Justified, Unified and contextually Sound.

Justified: If one or more Static Rules and one or more Causal Rules.

Unified: If Spatial, Conceptual and Static Unity achieved

Spatial Unity: Each object in the domain is directly or indirectly related to all other objects via Static Rules.

Conceptual Unity: Each predicate is constrained by mutual exclusion or by existence (there is at least one P(X)...) Integrity Rules.

Static unity: Applying the Static Rules expands, without integrity constraint violation, and validates each memory state of the Partal Sequence.

Temporal unity: Applying the Causal Rules on each completed state in the Partial Sequences generates the next completed state in the Partial Sequence (completed by Framing).

Framing: When generating a Sequence of states, a property is retained from the previous state if doing so does not violate any integrity rule.

Sound: Each Rule references, directly or indirectly, a belief of another GM without introducing circularity.

Predictor structure

    predictor(coverage(Count), variables([VariableDefinition,...]), static_rules([StaticRule, ...]), causal_rules([CausalRule,...]), integrity_rules([IntegrityRule, ...]).

    % Type of named variable used in rules that stand for an object in the GM's 
    % domain of this type. 
    variable_definition(Name, ObjectType).

    % Rules that infers elements of a state given other elements of that same state
    static_rule(if_all([sensed(PredicateName1, Variable1, ValueRange1), enacted(EffectorName1, ActionName1)...]),  then(sensed(PredicateName2, Variable2, ValueRange2))).

    % Rules that infer elements of a next state given elements of the prior state
    causal_rule(if_all([sensed(PredicateName, Variable, ValueRange), ...]),  then(sensed(PredicateName, Variable, ValueRange))).

    % Rule that constrain elements of any state to exist or not exist given other 
    % elements of that state
    integrity_rule(xor(sensed(PredicateName1, Variable1, ValueRange1), sensed(PredicateName2, Variable2, ValueRange2)). % PredicateName1 \= PredicateName2 | Variable1 \= Variable2

    integrity_rule(exists(sensed(PredicateName1, ObjectType1, ObjectType2))). 

Note: a ValueRange can be an enumeration of atoms (e.g. colors), a numerical range, or an object type.

Implied Integrity Rule as CHR constraint: sensed(PredicateName, Variable, ValueRange1), sensed(PredicateName, Variable, ValueRange2) ==> ValueRange1 = ValueRange2
