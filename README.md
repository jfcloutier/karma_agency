# Notes

## Requirements

Use the development version of SWI-PROLOG (https://www.swi-prolog.org/build/PPA.html)

```sh
% sudo apt-add-repository ppa:swi-prolog/devel
% sudo apt-get update
% sudo apt-get install swi-prolog
```

Install xterm

```sh
sudo apt-get install xterm
```

Define ~/.config/sw-prolog/init.pl

```prolog
:- set_prolog_flag(history, 50).
:- use_module(library(clpfd)).
:- use_module(library(threadutil)).
:- set_prolog_flag(double_quotes, chars).

ttrace(Thread) :-
    thread_signal(Thread, (attach_console, trace)).

tnotrace(Thread) :-
    thread_signal(Thread, (attach_console, notrace)).
```

## Tools

To debug a thread named `t1`, do

```prolog
% To use GUI tracer
guitracer.
% To instrument the thread for tracing
tdebug(t1).
% To initiate tracing on the thread, with output in an XTerm window
ttrace(t1).
% to stop tracing
tnotrace(t1).
% to dsiable debugging a trace
tnodebug(t1).
```

## Design

The following are design notes toward the implementation of karma . 

The raison d’être of  karma  is to explore how a functional implementation of Active Inference  can be imbued with a significant degree of Structure Learning. 

Learning in karma is achieved by growing and evolving an heterarchy of process-based generative models (GMs) where each GM is a software process that executes evolving logic programs to carry out its functions. The execution of these logic programs produce predictions (as assertions) given beliefs held (as inferred predicates) and Actions taken, interpret consequent prediction errors  into updated beliefs, and infer from revised beliefs action Policy(ies) that maintain the agent’s homeostasis. The logic programs are not predefined but are generated and evolved by each GM from interactions between the agent and its environment.  Learning is successful if the GMs, individually and as a heterarchy, get generally better over time at maintaining Homeostasis.

In this exploration of active inference, Generative models are not implemented as probability distributions over hidden states etc.; they are software processes that explicitly and quite literally carry out the functions expected of a predictive processing agent, namely making predictions, raising prediction errors, updating beliefs, and carrying out policies.  It is the outcomes of these functions on the dynamic state of an active inference agent and its environment that the Free Energy Principle models. 

Learning in karma  is thus not achieved by explicitly minimizing Variational Free Energy  but by updating data structures, generating executable (logic) code, and spawning software processes, i.e. by modifying the agent’s predictive processing machinery, and from this emerges the evolving behavior of the agent. 

It is hoped that agents implemented with karma will eventually be amenable to analysis using the tools of the Free Energy Principle and that these agents will be found to conform to the Free Energy Principle in that they actively minimize their Free Energy. 

About karma
karma  is a Prolog application exposed as a SWI-Prolog pengine accessed by a revised (Elixir) implementation of andy , an ActInf robotics framework targeting Lego EV3 robots.

Why the name:

"Karma is the causality principle focusing on 1)causes, 2)actions, 3)effects, where it is the mind's phenomena that guide the actions that the actor performs. Buddhism trains the actor's actions for continued and uncontrived virtuous outcomes aimed at reducing suffering."  See Wikipedia article.

karma  implements the learning capabilities of each "grown" GM, i.e. of any GM other than predefined Detectors (a Detector is a built-in GM that only believes its sensors and makes no predictions) or the Metacognition GM (responsible for instantiating and culling all grown GMs.)

For each such GM, karma  builds and revises logic programs that infer:

* The GM's beliefs from its perceptions (believer )
* Predictions based on beliefs held and actions taken (predictor )
* Action policies for achieving desired beliefs (enactor )

GM life cycle
A GM is instantiated by the Metacognition GM and given a unique identifier. If the new GM is instantiated as a competitor to another GM, it copies its initial scope from that GM, otherwise the new GM is responsible for setting its initial scope.

The scope of a GM is
* a set of typed objects adduced from the “hidden world”the GM holds beliefs and makes predictions about, for example, object(x1, obstacle), object(self, agent)
* a set of typed belief predicates, of arities 1 or 2, the GM uses to express the beliefs it holds and the predictions it makes, for example, belief_predicate(is_hitting, [agent, obstacle])
As part of its initialization, the new GM does the following moves:
* Its defines an initial scope
* It builds an initial believer 
* It builds an initial predictor 
* It builds an initial enactor 
As it interacts with its environment, an agent’s GMs evolve away from their initial states to capture what the agent has learned so far as it interacts with its environment in an attempt to maintain homeostasis.

The Metacognition GM might at some point remove a grown GM, typically because the GM no longer has children, or remove a GM competing with another because the other is performing consistently better.

The belief domain
The GM’s belief domain is  the set of typed belief predicates it uses to express the beliefs about objects in its umwelt, beliefs it infers from perceptions, which are themselves predictions about beliefs of other (”child”) GMs.

If the new GM was created by the Metacognition GM to compete with an existing grown GM, then the new GM started with a copy of the other GM’s belief domain, else the new GM is responsible for assigning itself a number of belief predicates as part of its initial scope.

A typed belief predicate of a GM can either be introduced by the GM (and added to a pool of belief predicates available to all GMs), or it can be taken from that pool. 

Belief predicates are used to express beliefs which are truth-valued statements about objects.

The perceptual domain
The perceptual domain of a GM constrains the predictions the GM can make about the beliefs held by other GMs.  A GM defines its perceptual domain by selecting belief predicates and objects from the vocabulary module.

Any GM that makes predictions about a belief potentially held by another GM is implicitly a parent of that other GM. GMs are organized by their perceptual domains into an evolving heterarchy.

The Umwelt
The local umwelt of a GM is the set of objects it can hold beliefs and make predictions about. A GM can perceive and hold beliefs about the agent but also about a number of other typed objects from its environment that are abduced from the hidden world the agent interacts with (its Umwelt).

If a GM was created by the Metacognition GM to compete with an existing grown GM, then the new GM copies its initial umwelt from it, else it is responsible for assigning itself an initial umwelt as part of its scope.

A GM can introduce new typed objects e.g. object(x1, type1), to its local umwelt and thus the umwelt of the agent. 

Generated logic
The  competence of a grown GM is contained in logic programs that are dynamically generated and revised from experience. The logic programs are constrained by the GM’s scope.

A GM implements four logic programs. Each is a generated Prolog module incorporating the GM's id in its module name to avoid namespace conflicts. 

A GM with id, say 1234, would have four Prolog modules.

* gm_1234 - This module defines the GM's scope and imports the other three "competency" modules: the GM‘s believer , predictor  and enactor .
* gm_believer_1234 - This module defines the rules with which the GM infers what it believes given what it perceives (predictions it made modulated by consequent prediction errors it received).
* gm_predictor_1234 - This module defines the rules with which the GM infers the predictions it makes given what it believes.
* gm_enactor_1234 -This module defines the rules with which the GM infers the actions it intends to take to modify its beliefs.

The scope-defining module (gm_1234 in this example) might be initially determined by the Metacognition GM should the GM be created to compete with another.

“Building blocks” that can be shared by GMs, for example, the objects composing an agent’s umwelt and the belief predicates a GM uses to express its beliefs and predictions, are contained (and updated) in the vocabulary module.

module vocabulary

:- module(vocabulary, [type/1, belief_predicate/2, object/2]).

% The types defined across all GMs
type(agent).
type(obstacle).
type(floor).
...

% The typed belief predicates from which a GM defines its belief domain
belief_predicate(distance_between, [agent, obstacle]).
belief_predicate(b1, [agent]).
...

% The typed objects (the agent's overall umwelt) from which GMs define their umwelts
object(self, agent).
object(x1, obstacle).
...


This is what the code generated for each type of module might look like:

 module gm_<id>


:- module(gm_<id>, [in_belief_domain/1, in_perceptual_domain/1, in_umwelt/1, believed/2, predicted/2, intended/1]).

:- reexport(gm_<id>_believer).
:- reexport(gm_<id>_predictor).
:- reexport(gm_<id>_enactor).

in_belief_domain(b10).
in_belief_domain(b11).

in_perceptual_domain(b1).
in_perceptual_domain(b2).
in_perceptual_domain(b3).

in_umwelt(self).
in_umwelt(x1).
in_umwelt(x2).

Module gm_<id>_believer

This module defines the rules from which what a GM believes is inferred.

The generation of a believer is subject to a number of constraints (more on this later), for examples, the belief domain is small, a belief and its contradiction must be provably mutually exclusive, and no two belief inference rules can have the exact same body.

%%% gm_<id>_believer %%%

:- module(gm_<id>_believer, [believed/2]).

% truth value beliefs, conditional on perceptions
believed(b10(self), is(true)) :- perceived(b2(x1), is(false)), perceived(b3(x1, x2), is(false)).
believed(b10(self), is(true)) :- perceived(b2(x1), is(true)), perceived(b2(x2), trends(decreasing)).
believed(b10(x1), is(false)) :- perceived(b2(x2), trends(is(true))).
believed(b11(self, x1), is(true)) :- ...
believed(b11(self, x1), is(false)) :- ...

% numerial value beliefs, always facts (held by Detectors only)
believed(b20(x2), is(10)).
believed(b21(self), is(red)).

The Object of a belief or a perception (believed/2, perceived/2) is an object in the GM’s umwelt.

The Value of a belief is the predicate is(Atom)  where Atom is either true or false, or an atom or number representing what a Detector detects (a color, a distance etc.)

The Value of a perceived/2 describes either an actual value (is/1) or a trend inferred from a history of perceptions (trends/1).  Examples of trends/1 predicates are trends(increasing), trends(decreasing), trends(stable), and trends(unknown). The trends/1 predicate can also be used to express an apparent attractor state, e.g. trends(is(true)) or trends(is(red)).

Module gm_<id>_predictor

This module defines the rules by which a GM infers its predictions, that is, what it anticipates other GMs to believe given its own current beliefs based on its perceptions and recently enacted intents.

A prediction is about a current belief of other GMs.

The rules are subject to constraints, such as, mutually contradicting predictions must be avoidable, e.g. predicted(b10, self, is(true)) and predicted(b10, self, is(false)) must not have the same preconditions.


:- module(gm_<id>_predictor, [predicted/2]).

% predicted(Belief, ValuePrediction) :- <beliefs>, <enactions>

predicted(b1(self), is(true)) :- believed(b10(x1), is(false)), believed(b11(x2), trends(increasing)).
predicted(b1(self), is(false)) :- believed(b11(self, x11), trends(is(true))), enacted(turn_right(small))
predicted(b2(self), is(false)) :- ...


Module gm_<id>_enactor

This module defines the rules from which a GM infers the actions it wants to take to reverse undesirable beliefs it holds, that is, beliefs that indicate deviation from homeostasis and allostasis (anticipated homeostasis), or that indicate lack of epistemic progress (deficient learning).

The rules are subject to constraints, such as, it must not be possible for mutually cancelling actions to be intended concurrently.

A GM's policy is the set of all currently inferred actions given a GM's beliefs. If the set is short, the ordering should mostly not matter, since no action in it cancels out another.

The GM's policy becomes candidate for execution. Concurrent policies from different GMs compete with one another if one contains an action that undoes another's action. Competing policies get prioritized, based on the urgency of the beliefs that caused them. Only one of a set of competing policies gets executed at any point in time. Prioritization and "gate-keeping" are effected outside of any one enactor .


:- module(gm_<id>_enactor, [intended/1]).

% intended(Action) :- Belief, ...

intended(turn_right(small)) :- believed(b1(self), is(true)).
intended(go_forward(big)) :- believed(b2(self), is(true)).
intended(go_backward(big)) :- believed(b2(self), is(false)).
...

Generating a GM’s logic programs
karma  implements an adapted Apperception Engine to search for a GM’s latest version of its logic programs. The search is triggered once the GM is no longer deemed sufficiently competent given accumulated experience.

The data used by the Apperception Engine to induce logic programs are:
* beliefs once held
* perceptions once had
* actions once taken

A GM's believer  is competent to the extent that it correctly models the GM's umwelt (indicated by how competent the predictor and enactor  are since they are defined in terms of the inferred beliefs), 

A GM's predictor  is competent to the extent that it makes predictions that are not contradicted by other GMs via prediction errors. 

A GM's enactor  is competent to the extent that the actions it intends, when they are taken, more or less immediately realize desirable beliefs (beliefs with a positive valence).

When revising the "competency logic programs" of a GM, karma  searches, in order, for

1. A more reliable predictor 
2. A believer  derived from the predictor 
3. A more effective enactor given the believer 

The Apperception Engine first searches for a "proto" predictor  with the shape:

predicted(ChildGMBeliefName1(Object1), Value1) :- perceived(ChildGMBeliefName2(Object1), Value2), perceived(ChildGMBeliefName3(Object1, Object2), Value3),...
...

A believer  is then induced by taking conjunctions in the bodies of the predictor  rules and using them to define beliefs. 

For example:

believed(BeliefName1(Object1), Value1) :- perceived(ChildGMBeliefName2(Object2), Value2), perceived(ChildGMBeliefName3(Object1, Object2), Value3).

The predictor  is then finalized by replacing all perceived/2 in its bodies by conjunctions of believed/2. 

For example:

predicted(ChildGMBeliefName1(Object1), Value1) :- believed(BeliefName1(Object1), Value2), ...

An attempt is made to resolve a new believer  that is as close as possible to the previous version.

The last step is searching for an enactor  given the possibly modified believer .

A GM's overarching objective is to achieve and preserve homeostasis within its purview. It does so by taking actions that successfully bring about desirable beliefs, beliefs contrary to those inferred from perceptions of "bad feelings" (negative valence beliefs).

karma  constructs an enactor  by 
* identifying the current beliefs of the GM that are inferred from perceptions of "bad feelings" (caused by deviation from homeostasis), 
* identifying actions that could cause the inverse belief, based on prior evidence, as available. 

No action that undoes one another must be inferable from any given belief.

Managing the searches

There are three levels of search spaces:
* the search space for generating a logic program, e.g. a GM's believer  (let's call it Level 1),
* the search space for generating the system of logic programs for a given GM, namely that GM's co-dependent believer , predictor  and enactor  (Level 2), from which emerges the GM’s competence,
* the search space for generating the heterarchy of GMs (Level 3), from which emerges the behavior of the agent.
If any search space is too large, finding an acceptable solution will, on average, require too much time and/or computing resources. It is critical to reduce the size of a search space. This is accomplished using heuristics such as imposing strong inductive biases. However overly constraining a search space prunes away many good solutions. A balance needs to be achieved.

The difficulty is compounded since we are arguably looking at a search space of search spaces of search spaces (Level 3 X Level 2 X Level 1)!

A search space can be constrained internally, for examples, by limiting the number of belief predicates a believer  can use, or by imposing that two contrary beliefs can be inferable but never from the same preconditions, etc.

A search space can also be constrained externally (search spaces constraining one another). For examples, the a GM's believer , predictor  and enactor  must use the same (small) set of belief predicates, and that two competing GMs must synchronize their belief domains, etc.

Defining search space constraints

The primary constraint might be restricting the number of GMs. The Metacognition GM should cautiously add new GMs and ruthlessly cull non-performing GMs. 

Another important type of constraint is one that determines the structure of the heterarchy of GMs. A GM has a position in the heterarchy of existing GMs. That position is determined by the GM’s perceptual domain. A GM’s perceptual domain is a set of belief predicates defined by other GMs and a GM’s beliefs are expressed in terms of its perceptual domain.  

The process of defining a perceptual domain is subject to constraints. For example, no two GMs can have identical perceptual domains, unless they are explicitly competing (no unintended duplication constraint.) 

The relative abstraction level of GMs in a heterarchy is defined in terms of a GM’s belief levels.

Let’s assert that a belief predicate from a Detector is a "level 1" belief, and that a belief defined in a GM solely in terms of “detected beliefs” a GM is a level 2 belief.  Let’s generalize and say that GM-defined belief has level 1 + the maximum level of its perceived beliefs. 

We can express constraints on a GM’s perceptual domain in terms of levels of its (predicted) beliefs such that:
* No belief is transitively defined in terms of itself, i.e. belief1 → prediction → belief2 → prediction → belief 1 is not allowed (no-cycle constraint) 
* The maximum difference between belief levels is 1 (abstraction-level constraint)
* A GM can not have a level N + 2 belief in its perceptual domain if no GM has a level N belief in its own perceptual domain (build-from-the-ground-up constraint)

The larger the scope of a GM, the larger will the search space be -exponentially so- for its logic programs. Keeping the scope of GM small is thus critical (focus constraint).

The scope of a GM consists of:
* its umwelt - the abduced objects it expresses beliefs or makes predictions about
* a set of predicate names to express beliefs about them
* a subset of other GMs beliefs to compose the GM's (predicted) perceptions
* a subset of actions the agent can intend to take as a consequence of its beliefs (can be empty)

Restricting the scope of a GM is accomplished by internally constraining it to:
* a small number of abduced objects beliefs/predictions can be about
* a small number of belief predicates it can use to express beliefs and predictions

Another mean is to constrain the internal complexity of the logic programs by:
* keeping each program to a small number of clauses (Occam’s razor constraint)
* keeping each clause body small (maximal generality constraint)

Further pruning of the search space can be achieved by imposing "unity of apperception" constraints:
* Spatial unity: objects are united in “space” by being transitively related via beliefs and predictions
* Conceptual unity: beliefs are united by constraints forbidding contradictions
* Static unity: static unity is achieved when perceptions supports beliefs instead of contradicting them
* Temporal unity: beliefs are united in time by tracking changes in perception

Let's examine what this means for a GM's believer , predictor  and enactor .

Unity of apperception constraints within a GM

Spatial unity

If the GM-introduced umwelt contains more than the implied self object, beliefs must be defined in the believer that associate all objects directly or indirectly. 

For example, the objects x1, x2 and self are spatially united by
 
...
believed(b1(self, x1), is(true)) :- ...
believed(b2(x1, x2), is(false)) :- ...
believed(b3(x1, is(true)) :- predicted(b4, x2, trends(is(increasing))), ...
...

Conceptual unity

Built-in constraints (not explicitly encoded in the believer ‘s clauses but encoded in the code generation logic) ensure conceptual unity.

For example,  two contradictory beliefs must not be provable, i.e.  believed(BeliefPredicate, Object, is_true(N)) => ~believed(BeliefPredicate, Object, is_false(N)).

The clauses in the predictor are also constrained so that inferring two mutually exclusive predictions must not be possible given any current beliefs.

Similarly, the clauses of the enactor must also not allow inferring mutually undoing actions given any current beliefs.

Static unity

Static unity is achieved by a GM at a given time if its current beliefs cause one or more predictions to be inferred and if those are not contradicted by prediction errors emitted by other GMs. 

Static unity is akin to model evidence. A GM strives to improve its model evidence by evolving its logic programs so as to minimize the ratio of prediction errors to predictions it makes.

Temporal unity

Temporal unity is automatically achieved because current beliefs cause predictions to be made, actions to be taken, prediction errors to be received, leading to revised beliefs, and so on.

Unity of apperception constraints across GMs

GMs form a heterarchy with Detectors at the “bottom”.  Just as there are unity of apperception constraints applicable to individual GMs, unity of apperception constraints also apply to the heterarchy of GMs as a whole.

Spatial unity

More abstract GMs impose spatial unity constraints on less abstract GMs. A GM’s perceptual domain must always map to the belief domains and umwelts of other (lower-level) GMs. It is meaningless for a GM, other than a Detector, to make predictions that can not be refuted by other GMs.

A GM whose belief domain and umwelt is referenced in another GM’s perceptual domain must not create “dangling references” when it modifies its scope.

Conceptual unity

Conceptual unity across GMs is achieved by multi-GM constraints, such as forbidding circular references in belief → prediction → belief ... chains in the heterarchy.

Static unity

A heterarchy of GMs achieves static unity to the extent that prediction errors are minimized over all GMs.

The static unity of the heterarchy of GMs corresponds to the model evidence of the agent as a whole, i.e. self-evidencing.

Temporal unity

Temporal unity of an agent is achieved by the joint evolution of its heterarchy of GMs toward self-evidencing, as supervised by the Metacognition GM.

The Metacognition GM
The Metacognition GM sits permanently “above” all other GMs. Before an agent starts learning, its “aggregate generative model” consists of the Metacognition GM and of the Detectors forming the initial, flat, heterarchy of GMs.

The Metacognition GM is responsible for “grooming” the heterarchy of GMs by:
* maximizing the heterarchy’s conceptual unity; the umwelt and belief domain of any given GM ought to be referenced in the perceptual domains of other GMs
* maximizing the heterarchy’s static unity, i.e. minimizing the ratio of prediction errors to predictions across GMs
* introducing competition among (initially identically scoped) GMs so that alternate evolutionary paths can be explored

This maximization leads to “model expansion” either by growing the scope of individual GMs or by causing new GMs to be added to the heterarchy.

This process of model expansion is constrained by:
* limits on the size of a GM’s scope imposed by karma 
* limits on the complexity of each logic program in a GM, also imposed by karma 
* associating a “metabolic cost” to the complexity of the heterarchy, thereby putting homeostatic (and allostatic) pressure on the agent to refrain from growing an “all-consuming brain”.

GMs added to the heterarchy behave as sensors to the Metacognition GM, reporting various metrics, such as:
* a measure of static unity (the GM’s model evidence)
* a measure of homeostasis (the overall valence of beliefs held - how little of what the GM believes is derived from negative feelings)
* a measure of stability (how the GM’s logic programs change over time)

Based on these sensory streams, the Metacognition GM derives beliefs about the fitness of the heterarchy of GMs and of individual GMs, makes predictions, receives prediction errors, updates its beliefs, and take actions (it is after all a GM).

The actions the Metacognition GM can take include:
* Adding a competing GM, giving it as initial scope the same scope as another GM
* Adding a GM de novo, letting it set its initial scope and finding its place in the heterarchy
* Removing an under-performing, redundant GM (redundant because removing it does not introduce “dangling predictions”)
* Removing an “orphaned” GM which scope is not referenced by other GMs

An under-performing GM is one reporting a combination of:
* consistently low static unity (low model evidence - high prediction errors to predictions ratio)
* consistently low homeostasis value (it is consistently failing to remove beliefs with negative valence)
* consistently high instability (evidence of “thrashing around in the search space” instead of steady learning)

The Metacognition GM is thus responsible for the expansion and contraction of an agent’s heterarchy of GMs, expanding it to increase unity and utility, and to explore alternate evolutionary paths, and shrinking the heterarchy by pruning under-performing GMs, thus reducing non-productive “metabolic costs”.

Explore/Exploit, Play and Dream

The agent is, at any time, in one of three modes 
* Explore/Exploit 
* Play 
* Dream

In Explore/Exploit mode, the agent is “all business” and is focused on maintaining homeostasis in an environment it perceives and acts in.

In Play mode, the survival imperative is relaxed and the focus is on acquiring more information about the efficacy of policies by selecting actions whose outcomes are currently opaque.

In Dream mode, the agent’s sensors are muted, the actuators disabled and an impoverished/unmoored sensory stream is simulated. The hypothesis is that “dreaming” will give an opportunity to the heterarchy of GMs to escape local minima in its aggregate search space.

The Metacognition GM determines when to put the agent in any of these modes.

Successfully implementing the Explore/Exploit mode is the first step after which the implementation of the other two modes will be attempted. 

