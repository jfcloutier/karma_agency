# Karma's Apperception Engine

## Predictor generator

Search for a predictor of what the GM is expected to sense next, given what it has sensed and done before.

A Predictor is a (generative) model of the GM's umwelt.

On its own, its has a temporal depth encompassing the remembered round, the current round and the next round.

Given a GM's sensory/enaction sequence(the GM's past and current perceptions by round + actions being executed by the agent during each round), search for a Predictor that produces a trace that covers preferably the sequence, else a sub-sequence (as long s possible), optimizing for simplicity of the Predictor (small scope, a few simple rules).

    Start with the current scope of the GM (object, sensory, belief and action domains)
    Possibly expand or contract the scope
    Express predictor rules within the scope
    Search for the simplest working predictor within a time limit

A working Predictor produces a trace that is a superset of the (sub)sequence it was derived from (a round may now contain inferred perceptions in addition to the remembered ones.)

It is possible for no Predictor to be found given a GM's remembered rounds, in which case the current Predictorm if any, is retained.

## Believer generator

Search for a Believer to synthesize beliefs from what a GM sensed/did according to its Predictor, and as it relates to what the agent feels.

    In other words, a GM's beliefs is a function of (is grounded in) the model of its relationship to its umwelt (its Predictor) and how the feelings of the agent change over time.

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

        Beliefs are associations between the dynamics of feelings (i.e. trends) and correlated sensings/enactings.

    Express these correlations as rules that each infer a belief from a set of sensings/enactings. 
    
    The rule associates the correlated feeling trend with the belief. This gives each belief a valence:
    
        Risks to homeostasis are 
            increasing (negative valence)
            decreasing (positive valence)
            staying the same (negative, positive or neutral valence).

It is possible for no Believer to be found given the current Predictor-produced trace. This is certainly the case if the GM has no Predictor yet.

## Enactor generator

Search for rules that infer policies likely to remove beliefs with negative valence and challenge beliefs with positive valence.

A policy is a sequence of intended actions, each to be taken in the context of beliefs held. 

An intended action may not be executed if it is overridden by another (intended by another GM) that conflicts with it and has higher priority.

Given the sensing/enacting trace produced by the Predictor (if any), overlayed with beliefs inferred by the Believer (if any):

    For each belief held in the sequence, 

        Find the segments, if any, bookended with the belief being held then lost.

        From the segments, find, if any, a common policy (each action was taken while common beliefs are held and it is found in all segments).

        Create a rule that associates a policy with losing a belief.

Enactor rules are used by the GM to determine if

    It is in the midst of executing a policy and, if so, what the next policy action to take is, given the beliefs held by the GM.

    A policy can start executing given a belief to be lost (it is held and has has negative valence) or challenged (it is held and has positive valence).

An Enactor accumulates rules found and drops one only if,

    it references a belief or action that has become out of scope of the GM,    

    a new rule for losing a belief replaces it.

Babbling: If a GM has no policy that addresses a belief that ought to be eliminated or challenged, it may choose (randomly, at a minimum) an action to enact. This will giev the GM "experience" from which it may later derive an Enactor.

## Temporal thickness and temporal depth

Temporal depth refers to how much of the past and the future is encompassed by a GM actor.

Temporal thickness refers to how long "now" lasts for a GM actor, that is, the amount of time during which perceptions are considered to be simultaneous by aGM actor.

### Temporal depth

The past's contribution to temporal depth depends on how many rounds a GM actor remembers. There is a sliding maximum number of rounds that can be remembered. A Detector remembers only the last round. A low-level GM actor (semantically close to Detector) remmembers fewer rounds than a high-level GM actor (semantically far from Detectors).

A Predictor, by itself, extends temporal depths into the future by predicting sensing theoretically to any number of future rounds, under the assumption that no action is executed during these anticipated future rounds. Practically, a Predictor, on its own, extends the fure by one round.

By applying the Predictor, Believer and Enactor to the current round and each consecutive anticipated round, a GM actor can iteratively create a trace of predicted perceptions with alternate planned actions, expected feeling trends, derived beliefs and consequenct action policies. A GM actor can imagine possible futures to any depth, limited only by its computing resources.

What would compel a GM actor to peer deeply vs shallowly into the future? 

When a GM actor has a choice of policies to execute, it needs to decide which one looks better by imagining the outcomes (like a chess player would). If the agent is at risk, risk aversion kicks in and so is future-gazing. Also, the more complex the policy, the deeper the projection needed to predict outcomes.

### Temporal thickness

The more abstract a GM actor (the furthest its sensory domain is from detected perceptions), the longer the duration of its rounds, i.e. the greater the temporal thickness of the GM actor.

A more abstract GM actor gathers model more integrated model evidence, and thus requires more time to pull perceptions together into a "scene".

An abstract GM actor thus proceeds more slowly from round to round than a more concrete one.

The temporal thickness of a GM actor is a function of its distance from Detectors, which is the longest sense->belief path to a Detector.

## Precision

The precision of a perception is the confidence a GM actor has in the uncontested prediction it made, or the average confidence other GM actors have in prediction errors they raised to contest the prediction.

The confidence in a prediction error is the confidence a GM actor has in the belief that contradicts the prediction.

The confidence a GM actor has in a prediction it makes is the same as the confidence the GM actor has in its Predictor (which instantiates its internal model of its umwelt). Its confidence is a function of:

    * the relative length of the sub-sequence from which the Predictor was derived (how much of its experience does the model encompass)
    * the success rate of the Predictor (uncontested predictions / all predictions)

The confidence a GM actor has in a belief in the current round is the multiplicative summation of the confidences in the perceptions that compose the belief, tempered with the confidence in the belief in prior rounds, the more recent rounds having greater influence (see Kalman filters.)

The valence of a belief (from associated feelings) is tempered by the confidence in the belief when computing the motivation behind an intended action caused by that belief. When conflicting actions are intended by GM actors, only the action with the highest motivation is executed.

## Attention

Attention is the general mechanism that determines the subset of GM actors that are active at any given pointin time.

A GM actor becomes dormant if

    * it is not babbling (a GM actor babbles until it acquires a Predictor, Believer and Enactor)
    * AND the valence of held beliefs is not altered by a change in agent feelings
    * AND no prediction is being made about beliefs in its scope for N consecutive rounds

Agent feelings shift whether or not GM actors are dormant.

A shift in feelings will activate certain GM actors but not others. The activation of a GM actor would lead to predictions being made, thus activating other GM actors that might hold the predcited beliefs.

The metacognition GM actor sees to it that the set of active GM actors does not stay or become empty for any significant length of time by

    * creating new GM actors with initial scopes chosen to fill "gaps" in the sensory domains
    * other?

## Babbling

A GM actor babbles, i.e. makes unprompted predictions and intends unprompted actions, while

    * it does not have a Predictor, Believer and Enactor
    * it is put temporarily in babbling mode via metacognition covert action

Babbling is a forces a GM actor to acquire experiences (past perceptions and actions) from which to induce a Predictor, Believer and Enactor, or to trigger their updates.

Once a GM actor is stimulated by other GM actors making predictions made about its beliefs, or is driven to action by its own beliefs, babbling may no longer be required.

If a GM actor's capabilities (Predictor, Believer, Enactor) are stuck in a rut (they do not evolve), a period of babbling might help generate experiences that will prompt revisions to its capabilities.

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
