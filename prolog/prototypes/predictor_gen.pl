% Generates the next predictor of a GM
:- module(predictor_gen, [next_predictor/3, complexity/2]).

:- use_module(library(chr)).
:- use_module(library(lists)).
:- use_module(sequence).
:- use_module(timed_search).
:- use_module(gm).
:- use_module(predictor).
:- use_module(scope).
:- use_module(vocabulary, [object/1]).

%% Search constraints
:- chr_option(check_guard_bindings, on).
:- chr_type list(T) ---> [] ; [T | list(T)].
:- chr_type boolean ---> true ; false.
:- chr_type kind ---> object ; predicate.
:- chr_constraint in_scope(+kind, +), extracting(+kind, +list), extract(+kind, -list),
                  is_in_scope(+kind, +, -boolean).
% Scoping
in_scope(Kind, Thing) \ in_scope(Kind, Thing) <=> true.
extracting(Kind, Things), in_scope(Kind, Thing)#passive <=> extracting(Kind, [Thing | Things]).
extract(Kind, _) ==> extracting(Kind, []).
extract(Kind, All)#passive, extracting(Kind, Extracted) <=> All = Extracted.
in_scope(Kind, Element) \ is_in_scope(Kind, Element, Boolean) <=> Boolean = true.
is_in_scope(_,_, Boolean) <=> Boolean = false.

next_predictor(GM, MaxSeconds, NextPredictor) :-
    enacted_sensory_sequence(GM, Sequence),
    % Search for predictors via backtracking until time expires
    search(
        predictor_gen:candidate_predictor(_, GM, Sequence), 
        predictor_gen: predictor_rating(_, _, Sequence), 
        MaxSeconds, 
        NextPredictor).

candidate_predictor(Predictor, GM, _) :-
    current_predictor(GM, Predictor).

candidate_predictor(Predictor, GM, Sequence) :-
    predictor_for_sequence(GM, Sequence, Predictor).

predictor_rating(Predictor, Rating, Sequence) :-
    complexity(Predictor.initial_conditions, CIC),
    complexity(Predictor.static_rules, CSR),
    complexity(Predictor.causal_rules, CCR),
    complexity(Predictor.constraints, CIR),
    Complexity is CIC + CSR + CCR + CIR,
    partial_coverage(Predictor, Sequence, PC),
    % TODO - Needs calibration
    Rating is PC / Complexity.

% How complex an arbitrary term is.
complexity([], 0) :- !.
complexity([Element | OtherElements], Complexity) :-
    !,
    complexity(Element, Complexity1),
    complexity(OtherElements, Complexity2),
    Complexity is Complexity1 + Complexity2.

complexity(Term, Complexity) :-
    compound(Term),
    !,
    Term =.. List,
    complexity(List, Complexity).

complexity(_, 1).

partial_coverage(Predictor, Sequence, PC) :-
    Predictor.coverage(Coverage),
    length(Sequence, Length),
    (Length = 0, PC = 0 ; PC is Coverage / Length).
    
% A predictor for a GM given a sequence of per-round perceptions (sensory sequence)
predictor_for_sequence(GM, Sequence, Predictor) :-
    % Choose a sub-sequence, prioritizing on recency then on length
    sub_sequence(SubSequence, Sequence),
    % Guess an object and sensory domain that covers the sub-sequence
    prediction_scope(GM, SubSequence, Scope),
    % List typed variables given the GM's scope
    variables(Scope, Variables),
    % Guess unity rules
    unity_rules(Scope, Variables, UnityRules),
    % Conceptual unity requirement met? (Each predicate in scope mentioned in a unity rule)
    conceptually_unified(Scope, UnityRules),
    % Guess static rules
    static_rules(Scope, Variables, StaticRules),
    % Guess causal rules
    causal_rules(Scope, Variables, CausalRules),
    % Guess initial conditions from the sub-sequence
    initial_conditions(SubSequence, InitialConditions),
    % Compute the trace (if computed, it implicitly meets static and temporal unity requirements)
    sensory_trace(InitialConditions, UnityRules, StaticRules, CausalRules, Trace),
    % Verify spatial unity requirement met (for each pair or objects in scope, there's a direct or indirect relation connecting them.)
    spatially_unified(Scope, Trace),
    % Verify that the Trace subsumes the sub-sequence
    sensory_trace_covers(Trace, SubSequence),
    % Assemble a new candidate predictor!
    length(SubSequence, Coverage),
    Predictor = predictor{
        coverage:Coverage, 
        scope:Scope,
        initial_conditions: InitialConditions, 
        variables:Variables, 
        unity_rules:UnityRules, 
        static_rules:StaticRules, 
        causal_rules:CausalRules}.

% Alter (reduce or expand) the current GM's sensory and object scope to cover for the sub-sequence
prediction_scope(GM, SubSequence, PredictionScope) :-
    check_time(),
    assume_minimum_domain(SubSequence),
    extend_object_domain(),
    extend_sensory_domain(),
    extract_prediction_scope(PredictionScope).

assume_minimum_domain([]).
assume_minimum_domain([enacted(_) | Rest]) :- assume_minimum_domain(Rest).
assume_minimum_domain([sensed(Sensed, is(Value)) | Rest]) :-
     Sensed =.. [Predicate, Object],
     in_scope(predicate, Predicate),
     in_scope(object, Object),
    (object(Value), in_scope(object, Value) ; true),
    assume_minimum_domain(Rest).

% TODO - grow the object domain with an existing object or a create a new one
extend_object_domain().
% TODO - grow the sensory domain with a belief predicate from another GM without introducing sensing-believing circularity
extend_sensory_domain().

extract_prediction_scope(SensoryScope) :-
    extract(object, Objects),
    extract(predicate, Predicates),
    object_domain(Objects, ObjectDomain),
    sensory_domain(Predicates, SensoryDomain),
    PredictionScope = scope{object_domain:ObjectDomain, sensory_domain:SensoryDomain, belief_domain:[], action_domain:[]}.

% TODO
variables(Scope, Variables).

% TODO
unity_rules(Scope, Variables, UnityRules).

% TODO
conceptually_unified(Scope, UnityRules).

%TODO
static_rules(Scope, Variables, StaticRules).

% TODO
causal_rules(Scope, Variables, CausalRules).

% TODO
extract_prediction_scope(SensoryScope).

% TODO
initial_conditions(SubSequence, InitialConditions).

% TODO
sensory_trace(InitialConditions, UnityRules, StaticRules, CausalRules, Trace).

% TODO
spatially_unified(Scope, Trace).

% TODO
sensory_trace_covers(Trace, SubSequence).
