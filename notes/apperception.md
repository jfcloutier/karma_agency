# Karma's Apperception Engine

## Predictor generator

Search for a better predictor of what the GM senses next, given what it has sensed and done before.

Given a GM's sensory/enaction sequence, search for a predictor that produces a trace that best covers the sensory sequence, optimizng for both coverage and "predictor cost".

    Select initial state as the first element of the sequence (empty only when the sequence itself is empty)
    Start with the current scope of the GM (object, sensory, belief and action domains)
    Express predictor rules in terms of object, sensory and action domains only (substitue beliefs with senses)
    Search for modifications (starting with no modification) of the GM's definition that maximize predictive performance and minimize cost.

## Belief generator

Search for better sense-making (belief inferencing) of what a GM senses so that it can act beneficially.

## Policy generator

Search for better action policies so that beliefs with negative valence are reliably lost and beliefs with positive valence are effectively challenged.

=======================

A predictor is a set of static and dynamic rules, obeying constraints.

The static rules affirm and complete each state in a trace of incomplete states given what's known about the state => static unity.

The causal rules describe how a completed state determines the next state with frame axiom (properties in state N-1 not incompossible with state N are retained) => temporal unity

Constraints on static rules impose that each object in the object domain be related via binary predicates, directly or indirectly, to each other object => spatial unity

Constraints on static rules impose that each n-ary predicate be subject to exclusive disjunction - p1(X) (x) p2(X) - or (for binary predicates) unique existence - for all X there is always a Y such that p3(X, Y) - => conceptual unity

    Exclusive disjunction p1(X) (x) p2(X) means one of p1(X) or p2(X) MUST be true and never both

Input:
    Sensory-enaction sequence
    Type signature
    Initial conditions
    At least one constraint per property

Template:
    Type signature (set of types plus typed objects, properties and variables) that satisfies the sequence (captured trace)
    Ns: Max number of static rules
    Nc: Max number of causal rules
    Nr: Max size of any rule (number of conjunctions)

Computation:
    Iterating over generated templates of increasing complexity, until time limit is exhausted do
        Find a valid theory (if any) for the template
            If found, calculate its cost
    Keep the theory with lowest cost

Template generation:
    First enumerate sets T of types X N (for upper limit of Template(T)  - (O, P, V, Ns, Nc, Nr) given T
    For each (T, N), enumerate N Template(T)

Enumerating over N Template(T)s:
    Os = list of possible, satisfiable O given T, in increasing order of cardinality
    Ps = list of possible, satisfiable P given T, in increasing order of cardinality
    Vs = list of possible, satisfiable V given T, in increasing order of cardinality
    N* = {0,1,2...}

    Enumerate over the cardinal product of Os x Ps x Vs x Ns x Nc X Nr

Any system that does invoke latent information beneath the surface of the sensory stimulations must also
define the initial values of the latent information.
