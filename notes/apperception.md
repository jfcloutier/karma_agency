# Karma's Apperception Engine

## Predictor generator

Search for a predictor of what the GM senses next, given what it has sensed and done before.

A Predictor instantiates a (generative) model of the GM's umwelt.

Predictor's temporal depth is round N -> N + 1.

Given a GM's sensory/enaction sequence, search for a Predictor that produces a trace that covers preferably the sequence, else a sub-sequence, optimizing for simplicity of the Predictor (small scope, a few simple rules).

    Start with the current scope of the GM (object, sensory, belief and action domains)
    Possibly expand or contract the scope
    Express predictor rules in terms of object, sensory and action domains within the scope
    Search for the simplest working predictor in a set amount of time

A working Predictor produces a trace that is a superset of the (sub)sequence it was derived from (a round may now contain inferred perceptions in addition to the remembered ones.)

It is possible for no Predictor to be found given a GM's remembered rounds, in which case the current Predictorm if any, is retained.

## Believer generator

Search for a Believer to synthesize beliefs from what a GM sensed/did according to its Predictor, and as it relates to what the agent feels.

    In other words, a GM's beliefs is a function of the model of its relationship to its umwelt (its Predictor) and how the feelings of the agent change over time.

    The GM bases its actions on what it believes, and its beliefs provide synthetic perceptions for more abstract GMs.

A Believer's temporal depth extends from the present to the remembered past of a GM encompassed by the current Predictor.

Given the sensing/enacting trace (sub-sequence augmented with implied sensings) produced by the current Predictor, if any.

    Overlay the trace with the recorded agent feelings in each covered round.

    Overlay the trace with detected sensing trends (upward, downward, static, alternating).

    Overlay the trace with detected feeling trends (upward, downward, static, alternating).

    Determine the scope of beliefs, constrained by the sensory scopes of higher GMs
        
        Don't remove belief predicates in the sensory domain of other GMs

        Possibly add (abduce) new belief predicates

    Find, in the overlayed trace, correlations between sets of sensings/enactings (points and trends) and feeling trends.

        Beliefs are associations between the dynamics of feelings and correlated sensings/enactings.

    Express these correlations as rules that each infer a belief from a set of sensings/enactings. The rule associates the correlated feeling trend with the belief. This gives each belief a valence:
    
        Risks to homeostatis are 
            increasing (negative valence)
            decreasing (positive valence)
            staying the same (neutral valence).

It is possible for no Believer to be found given the current Predictor-produced trace. This is certainly the case if the GM has no Predictor yet.

## Enactor generator

Search for rules that infer policies likely to remove beliefs with negative valence and challenge beliefs with positive valence.

A policy is a sequence of actions, each to be taken in the context of beliefs held.

Given the sensing/enacting trace produced by the Predictor (if any), overlayed with beliefs inferred by the Believer (if any):

    For each belief held in the sequence, 

        Find the segments, if any, where the belief is held then lost.

        From the segments, find, if any, a common policy (each action was taken while common beliefs are held and is found in all segments).

        Create a rule that associates a policy with losing a belief.

Enactor rules are used by the GM to determine if

    It is in the midst of executing a policy and, if so, what the next policy action to take is, given the beliefs held by the GM.

    A policy can start executing given a belief to be lost (it is held and has has negative valence) or challenged (it is held and has positive valence).

An Enactor accumulates rules found and drops one only if..

    it references a belief or action that has become out of scope of the GM,    

    a new rule for losing a belief replaces it.

Babbling: If a GM has no policy that addresses a belief to be eliminated or challenged, it may choose (randomly, at a minimum) an action to enact.

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
