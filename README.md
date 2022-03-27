# karma (under construction)

*karma* is a Prolog application exposed as a [SWI-Prolog](https://www.swi-prolog.org/) [pengine](https://pengines.swi-prolog.org/docs/index.html).

["Karma is the causality principle focusing on 1)causes, 2)actions, 3)effects, where it is the mind's phenomena that guide the actions that the actor performs. Buddhism trains the actor's actions for continued and uncontrived virtuous outcomes aimed at reducing suffering."](https://en.wikipedia.org/wiki/Causality#Buddhist_philosophy)

`karma` implements the learning capabilities of each "grown" GM, i.e. any GM other than pre-defined Detectors (a Detector is a built-in GM that only believes its sensors and makes no predictions)or the MetaCognition GM (responsible for growing and culling all other GMs.)

For each GM, `karma` induces logic programs that infer:

* The GM's beliefs from its current perceptions (`believer`)
* Predictions based on beliefs held and actions taken (`predictor`)
* Action policies for achieving or testing beliefs (`enactor`)

## Grown GM lifecycle

A GM is instantiated by the MetaCognition GM, given a unique identifier, and possibly the identity of another grown GM to compete with.

As part of its initialization, the new GM does these moves:

* It defines predicates for its own beliefs (its belief domain); their values will be infered from the GM's perceptions.
* It selects beliefs predicates from other GMs that will compose its perceptual domain.
* It builds an initial `believer`
* It builds an initial `predictor`
* It builds an initial `enactor`

A grown GM might later be removed by the MetaCognition GM, typically because the GM no longer has children.

### Defining the belief domain

The second move of a new GM's self-initialization is the selection of its belief domain, the set of belief predicates to be used by the GM to express
its beliefs infered from its perceptions, which are the predicted beliefs of its child GMs.

If the new GM was created by the MetaCognition GM to compete with an existing grown GM, then the new GM simply copies its belief domain from it, else
it must define it by creating a number of belief predicates.

Constraints on the creation of belief predicates:

* A belief set contain from 1 to 3 predicates
* A new belief predicate is unique across the belief domains of all GMs.

### Defining the perceptual domain

Belief predicates are more or less abstract, depending on the position in the heterachy of GMs of the GM that defines them. A belief predicate from a Detector is a "level 1" belief. 
A belief predicate defined in a GM has a level 1 + the maximum level of any of its perceived beliefs.

Constraints on the selection of "perceived belief predicates":

* A perceptual set contains from 1 to 3 belief predicates
* No GM parent-child cycle is created
* Only one transitive path from a GM to any of its children
* No two GMs have identical perceptual sets
* A level N + 2 belief predicate can not be selected if there is a level N belief predicate yet to be selected by a GM

Any GM, including Detectors, that defines a belief predicate in the perceptual domain of another GM is implicitly a child of that other GM.
The new GM's first move thus positions it in the heterachy of GMs and Detectors.

## Implementation

### module gm_<id>

%%% Dynamically created with %%%

:- assert(gm<id>, belief_domain([b10, b11])).
:- assert(gm<id>, perceptual_domain([b1, b2, b3])).
:- gm<id>:import(gm_<id>_believer:believed/2).
:- gm<id>:import(gm_<id>_predictor:predicted/2).
:- gm<id>:import(gm_<id>_enactor:intended/2).
:- gm<id>:export(belief_domain/1).
:- gm<id>:export(perceptual_domain/1).
:- gm<id>:export(believed/3).
:- gm<id>:export(predicted/2).
:- gm<id>:export(intended/2).

%%% Equivalent to static %%%

:- module(gm_<id>, [belief_domain/1, perceptual_domain/2,]).

:- reexport(gm_<id>_believer).
:- reexport(gm_<id>_predictor).
:- reexport(gm_<id>_enactor).

belief_domain([b10,b11])
perceptual_domain([b1, b2, b3])

### Module gm_<id>_believer

A belief and its contradiction (e.g. b10(true) and b10(false)) must be provably mutually exclusive.
It is acceptable for rules to have incomplete coverage, i.e. that it be possible that neither b10(true) or b10(false) be provable.
Residual beliefs are beliefs previously held that are not replaced because no value of the belief is currently provable.

%%% gm_<id>_believer %%%

:- module(gm_<id>_believer, [believed/3]).

:- use_module(predictor).

% believed(BeliefName) :- <perceptions>

% truth value beliefs, conditional on perceptions
believed(b10(true)) :- believed(b2, is(small)), believed(b3, is(big)).
believed(b10(true)) :- believed(b2, is(small)), believed(b2, trends(decreasing)).
believed(b10(false)) :- believed(b2, is(big)).
believed(b11(true)) :- ...
believed(b11(false)) :- ...

% numerial value beliefs, always facts (held by Detectors only)
believed(b1, 10).
believed(b2, red).

### Module gm_<id>_predictor

What a GM anticipates child GMs to believe given its current beliefs (provable, residual) and its past beliefs.

Two mutually contradicting predictions must not be possible, e.g. proving predicted(b10(is(true))) and predicted(b10(is(false))) 
must not be possible in any context.

All provable predictions are made.

% predicted(BeliefName, ValuePrediction)

%% truth value predictions
% immediate
predicted(b10(is(true))) :- believed(b20, trends(false)), believed(b21, is(true)).
predicted(b10(is(true))) :- ...
predicted(b10(is(false))) :- ...
% over time
predicted(b10(trends(true))) :- ...
predicted(b10(trends(false))) :- ...

%% number value predictions
% immediate
predicted(b2, is(small)) :- ...
predicted(b2, is(big)) :- ...
% over time
predicted(b2, trends(small)) :- ...
predicted(b2, trends(big)) :- ...

%% number value change predictions
% immediate
predicted(b2, is(same)) :- ...
predicted(b2, is(increasing)) :- ...
predicted(b2, is(decreasing)) :- ...
% over time
predicted(b2, trends(same)) :- ...
predicted(b2, trends(increasing)) :- ...
predicted(b2, trends(decreasing)) :- ...

### Module gm_<id>_enactor


%%% gm_<id>_enactor %%%

### Believer

belief(Name, Value), where Value is a boolean or a number

A (non-Detector) GM defines a domain of boolean beliefs. Only a Detector can define numerical beliefs.

belief(Name1, true) XOR belief(Name1 , false)

`Karma` induces a GM's `believer` as a logic program that infers each of the GM's beliefs (belief_1, belief_2, ...) as `belief(Name, true)`) and its negation (`belief(Name, false)`) 
as a conjunction of > 1 perceptions (beliefs of child GMs) such that `belief(Name, true)` XOR `belief(Name, false)`, and `belief(Name1, Boolean)` XOR `belief(Name2, Boolean)` if Name1 != Name2.

The body of the belief rules are conjunctions of perception(Name, Value) or trend(perception(Name), Trend)  where Trend in [increases, decreases, stable, chaotic]

### Predictor

`Karma` implements an adapted [Apperception Engine](https://arxiv.org/pdf/1910.02227.pdf) which generates predictive logic programs from history.

* Given past perceptions and actions, generates and updates a logic program (a `predictor`) that infers the next perceptions (i.e. makes predictions about the beliefs of child GMs)

### Enactor

...


===========================================

The generated logic program embodies a cause-effect model. It is fit if, given past and current beliefs and actions, it

1. infers effective actions to take in the current round to achieve target beliefs in the next round
2. retrodicts belief changes in past rounds given prior actions taken at the time

In other words, the predictor, given the beliefs at round N and the beliefs at round N + 1, infers what actions to take to achieve them. If the agent is currently at round N then the logic program infers what actions to take to achieve desired beliefs (goals to be achieved, opinions to be preserved). If the predictor is otherwise applied to a past round N, then it should correctly infer the known beliefs at round N + 1, given the beliefs and actions taken at round N.

Once the predictor's performance becomes unacceptable, it is re-generated.

### Generation

Karma is a Prolog application that induces a `predictor` for an associated Generative Model.

The input to Karma from Andy is a sequence of data from remembered rounds of the associated GM:

1. The beliefs held at the end of the round, as `belief(RoundIndex, ConjectureName, AboutObject, GoalOrOpinion, Value)` predicates
2. The actions taken at the end of the round, as `action(RoundIndex, Type, IntentName, Value, Duration)` predicates
3. The rounds remembered, as `round(RoundIndex, WhenStarted, WhenCompleted)`

The value of a belief can be unitary (e.g. a number) or composite (e.g. a predicate)
The type of an action is an element of a pre-defined set, e.g. {locomotion, handling, vocalisation}

Round data informs the search for a predictor (as cause-effect model) that satisfies the data.

An agent's domain of action (intent names, parameters domains and limits on duration) is pre-defined and fixed. It defines what the agent can do. The domain of beliefs (conjecture names and value domains) for an agent is open-ended. A "seed" subset is predefined. In other words, Karma can invent conjecture names in its search for a predictor. Invented conjecture names (a, b, c etc.) might correspond to conditions such as "wheel slippage" or "unseen obstacle behind" etc. that are not in the GM's predefined domain of conjectures.

Karma searches a constrained space of logic programs for one that fits the input data (it can predict and it can retrodict action-belief cause and effect). The search space is contrained by Immanuel Kant's rules of "synthetic unity of apperception", and by time spent and generated code complexity. The fitness function for the predictor is expressed in terms of the predictor's accuracy and complexity (the smaller the program, the better).

Unity of apperception constraints:

1. Spatial unity: objects are united in space by being connected via chains of relations
2. Conceptual unity: predicates are united by constraints (.e.g. p(N) => ~q(N))
3. Static unity: atoms are united in a state by jointly satisfying constraints and static rules
4. Temporal unity: states are united in a sequence by causal rules

* A predictor can never contain more than one belief predicate with the same conjecture name for a given round that's about the same individual.
* TBD

The best candidate predictor logic program is stored in the associated GM's pengine as a Prolog module named `predictor`.

### Legacy rules

A GM keeps only a certain number of past rounds in memory, with older ones eventually forgotten. These forgotten rounds are no longer available to participate in the generation of the predictor logic program, or in its validation. Important "rules" may have been learned in forgotten rounds because of possibly rare but crucial circumstances not encountered since. The logic program will conserve "rules" for as long as possible across versions of itself, even though they may no longer retrodict the retained experiences.

A new version of a logic program will only shed "legacy rules" from prior versions if

1. they make it fail to predict/retrodict experiences from the retained rounds
2. they make the program exceed complexity limits
