# karma (under construction)

*karma* is a Prolog application exposed as a [SWI-Prolog](https://www.swi-prolog.org/) [pengine](https://pengines.swi-prolog.org/docs/index.html).

About the name:

["Karma is the causality principle focusing on 1)causes, 2)actions, 3)effects, where it is the mind's phenomena that guide the actions that the actor performs. Buddhism trains the actor's actions for continued and uncontrived virtuous outcomes aimed at reducing suffering."](https://en.wikipedia.org/wiki/Causality#Buddhist_philosophy)

`karma` implements the learning capabilities of each "grown" GM, i.e. any GM other than pre-defined Detectors (a Detector is a built-in GM that only believes its sensors and makes no predictions)or the MetaCognition GM (responsible for growing and culling all other GMs.)

For each GM, `karma` induces logic programs that infer:

* The GM's beliefs from its current perceptions (`believer`)
* Predictions based on beliefs held and actions taken (`predictor`)
* Action policies for achieving or testing beliefs (`enactor`)

## Grown GM lifecycle

A GM is instantiated by the MetaCognition GM, given a unique identifier, and possibly the identity of another grown GM to compete with.

As part of its initialization, the new GM does these moves:

* It defines predicates for its own beliefs (its belief domain); their values will be inferred from the GM's perceptions.
* It selects beliefs predicates from other GMs that will compose its perceptual domain.
* It builds an initial `believer`
* It builds an initial `predictor`
* It builds an initial `enactor`

A grown GM might later be removed by the MetaCognition GM, typically because the GM no longer has children.

### Defining the belief domain

The second move of a new GM's self-initialization is the selection of its belief domain, the set of belief predicates to be used by the GM to express
its beliefs inferred from its perceptions, which are the predicted beliefs of its child GMs.

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

## Generated logic

The competence of a grown GM is implemented by logic programs generated and revised by `karma` from an initial scope.
This scope, the GM's belief and perceptual domains, are set by the Metacognition GM, subject to the above constraints
The GM is assigned a unique ID.

Four logic programs define the GM. Each is a generated Prolog module incoporating the GM's id in its name to
avoid namespace conflicts. A GM with id, say 1234, would cause four Prolog modules to be generated:

* gm_1234 - This module defines the GM's scope and imports the other three "competency" modules: the `believer`, the `predictor` and the `enactor`.
* gm_believer_1234 - This module defines the rules with which the GM infers its beliefs given what it perceives (predictions made combined with prediction errors received).
* gm_predictor_1234 - This module defines the rules with which the GM infers the predictions it makes given what it believes.
* gm_enactor_1234 -This module defines the rules with which the GM infers the actions it intends to take after its predictions are processed and its beliefs revised.

The scope-defining module (gm_1234 in this example) is fully determined by the Metacognition GM. The other three modules are generated and revised by `karma`'s
apperception engine.

Follows a description of the code generated for each type of module.

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

A belief and its contradiction, for e.g. believed(b10, is(true)) and believed(b10, is(false)), must be provably mutually exclusive.
No two belief inference rules can have the exact same body.
It is acceptable for rules to have incomplete coverage, i.e. that it be possible that neither b10(true) or b10(false) be provable.
Residual beliefs are beliefs previously held that are not replaced because no value of the belief is currently provable.

%%% gm_<id>_believer %%%

:- module(gm_<id>_believer, [believed/2]).

% believed(BeliefName, Value) :- <perceptions>

% truth value beliefs, conditional on perceptions
believed(b10, is(true)) :- perceived(b2, is(small)), perceived(b3, is(big)).
believed(b10, is(true)) :- perceived(b2, is(small)), perceived(b2, trends(decreasing)).
believed(b10, is(false)) :- perceived(b2, is(big)).
believed(b11, is(true)) :- ...
believed(b11, is(false)) :- ...

% numerial value beliefs, always facts (held by Detectors only)
believed(b1, is(10)).
believed(b2, is(red)).

% perceived(BeliefName, Value) is inferred from the GM's history of perceptions.

### Module gm_<id>_predictor

What a GM anticipates child GMs to believe given its current beliefs (provable, residual) and recently enacted intents.

Mutually contradicting predictions must not be possible, e.g. proving predicted(b10(is(true))) and predicted(b10(is(false)))
must not be possible in any context.

All provable predictions are made.

:- module(gm_<id>_predictor, [predicted/2]).

% predicted(BeliefName, ValuePrediction) :- <beliefs>, <enactions>

%% truth value predictions
% immediate
predicted(b10, is(true)) :- believed(b20, is(false)), believed(b21, is(true)).
predicted(b10, is(true)) :- believed(b21, is(true)), enacted(turn_right(small))
predicted(b10(is(false))) :- ...
% over time
predicted(b10, trends(true)) :- ...
predicted(b10, trends(false)) :- ...
predicted(b10, trends(unknown)) :- ...
%% number value predictions
% immediate
predicted(b2, is(small)) :- ...
predicted(b2, is(big)) :- ...
% over time
predicted(b2, trends(small)) :- ...
predicted(b2, trends(big)) :- ...
predicted(b2, trends(big)) :- ...
predicted(b2, trends(unknown)) :- ...

%% number value change predictions
% immediate
predicted(b2, is(same)) :- ...
predicted(b2, is(increasing)) :- ...
predicted(b2, is(decreasing)) :- ...
% over time
predicted(b2, trends(same)) :- ...
predicted(b2, trends(increasing)) :- ...
predicted(b2, trends(decreasing)) :- ...
predicted(b2, trends(none))) :- ...

### Module gm_<id>_enactor

A GM wants to reverse beliefs it holds that indicate deviation from homeostatis, now or anticipated, or indicate lack of epistemic progress (deficient learning).

It must not be possible for mutually cancelling actions to be intended.

A GM's policy is the set of all currently inferable actions given a GM's beliefs. If the set is short, the ordering should mostly not matter, since no action in it cancels out another.

The GM's policy becomes candidate for execution. Concurrent policies from different GMs might compete with one another (one contains an action that undoes another's action). Competing policies get prioritized, based on the urgency of the beliefs that caused them. Only one of competing policies gets executed at any point in time. Prioritization and "gate-keeping" are effected outside of any one `enactor`.

:- module(gm_<id>_enactor, [intended/1]).

% intended(Action) :- Belief, ...

intended(turn_right(small)) :- believed(b10, is(true)).
intended(go_forward(big)) :- believed(b10, is(true)).
...

## Generating a GM's logic programs

`karma` implements an adapted [Apperception Engine](https://arxiv.org/pdf/1910.02227.pdf) to search for competent logic programs, competent in that, given prior states, they can predict the next state, as well as retrodict prior incompletely described states. A state can be the GM's beliefs, perceptions (uncontradicted predictions made plus prediction errors received) or intentions (to take action).

The `gm<id>` logic program (of the GM with ID == id) is fully determined by the input given at creation by the Metacognition GM and does not rely on the Apperception Engine.

The `gm<id>_believer`, `gm<id>_predictor` and `gm<id>_enactor`, however, are searched for from data accumulated during the recent past of the GM.

This historical data that describe prior states are:

* beliefs once held
* perceptions once had
* actions once intended and taken

A GM's `believer` is competent to the extent that it correctly models the GM's umwelt. A GM's `predictor` is competent to the extent that it makes predictions that are not contradicted by prediction errors. A GM's `enactor` is competent to the extent that the actions it intends, when they are taken, realize or validate beliefs.

When revising the "competency logic programs" of a GM, `karma` searches, in order, for

1. A more reliable `predictor`
2. A `believer` implied by the `predictor`
3. A more effective `enactor`

The Apperception Engine searches for a "proto" `predictor` with the shape:

predicted(ChildGMBeliefName1, Value1) :- perceived(ChildGMBeliefName2, Value2), perceived(ChildGMBeliefName3, Value3),...
...

A `believer` is then searched by taking conjunctions in the bodies of the `predictor` rules and using them to define beliefs. For example:

believed(BeliefName1) :- perceived(ChildGMBeliefName2, Value2), perceived(ChildGMBeliefName3, Value3).

The `predictor` is then finalized by replacing all perceived/2 in its bodies by conjunctions of believed/2. For example:

predicted(ChildGMBeliefName1, Value1) :- believed(BeliefName1), ...

An attempt is made to resolve a new `believer` that is as close as possible to the previous version.

The last step is searching for an `enactor` given the possibly modified `believer`.

A GM's overarching objective is to achieve and preserve homeostasis as afforded by its purview. It does so by taking actions that likely bring about beliefs contrary to those inferred from perceptions of "bad feelings".

`karma` constructs an `enactor` by identifying the current beliefs of the GM that are inferred from perceptions of "bad feelings" (caused by deviation from homeostatis),
and by identifying actions that could cause the inverse belief, based on prior evidence as available. No actions that undo one another must be inferred from any given belief.

### Managing the search

There are three levels of search spaces:

* the search space for generating one logic program, e.g. a GM's `believer` (let's call it Level 1),
* the search space for generating the system of logic programs for a given GM, namely the GM's `believer`, `predictor` and `enactor` (Level 2), from which emerges the behavior of a GM,
* the search space for generating the system of all systems of GM logic programs (Level 3), from which emerges the behavior of the agent.

If a search space is too large, finding an acceptable solution will, on average, require too much computing resources. It is thus critical to reduce the size of a search space through heuristics such as inductive biases. However overly constraining a search space prunes away many good solutions. A balance needs to be achieved.

The difficulty is compounded since we are arguably looking at a search space of search spaces of search spaces (Level 3 X Level 2 X Level 1)!

A search space can be constrained internally, for examples, by limiting the number of belief predicates a `believer` can use, or by imposing that two contrary beliefs must be inferrable but never from the same preconditions, etc.

A search space can also be constrained externally (one search space constraining another), for examples, a GM's `believer`, `predictor` and `enactor` must use the same (small) set of belief predicates, or two competing GMs must use the same set of belief predicates, etc.

### Defining search space constraints

<WORK IN PROGRESS>




A
 for one that fits the input data (it can predict and it can retrodict action-belief cause and effect). The search space is contrained by Immanuel Kant's rules of "synthetic unity of apperception", and by time spent and generated code complexity. The fitness function for, say, a predictor is expressed in terms of the predictor's accuracy and complexity (the smaller the program, the better).

Unity of apperception constraints:

1. Spatial unity: objects are united in space by being connected via chains of relations
2. Conceptual unity: predicates are united by constraints (.e.g. p(N) => ~q(N))
3. Static unity: atoms are united in a state by jointly satisfying constraints and static rules
4. Temporal unity: states are united in a sequence by causal rules

### Legacy rules

A GM keeps only a certain number of past rounds in memory, with older ones eventually forgotten. These forgotten rounds are no longer available to participate in the generation of the predictor logic program, or in its validation. Important "rules" may have been learned in forgotten rounds because of possibly rare but crucial circumstances not encountered since. The logic program will conserve "rules" for as long as possible across versions of itself, even though they may no longer retrodict the retained experiences.

A new version of a logic program will only shed "legacy rules" from prior versions if

1. they make it fail to predict/retrodict experiences from the retained rounds
2. they make the program exceed complexity limits
