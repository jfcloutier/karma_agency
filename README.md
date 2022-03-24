# karma (under construction)

*karma* is a Prolog application exposed as a [SWI-Prolog](https://www.swi-prolog.org/) [pengine](https://pengines.swi-prolog.org/docs/index.html).

["Karma is the causality principle focusing on 1)causes, 2)actions, 3)effects, where it is the mind's phenomena that guide the actions that the actor performs. Buddhism trains the actor's actions for continued and uncontrived virtuous outcomes aimed at reducing suffering."](https://en.wikipedia.org/wiki/Causality#Buddhist_philosophy)

`Karma` implements the learning capabilities of each "grown" GM, i.e. any GM other than Detectors or the Meta Cognition GM which are "built-in".

For each GM, it induces logic programs that infer:

* The GM's beliefs from its current perceptions (`believer`)
* Predictions based on beliefs held and actions taken (`predictor`)
* Action policies for achieving or testing beliefs (`policy`)

## Grown GM lifecycle

A GM is instantiated by the Meta Cognition GM and given a unique identifier. As part of its initialization, the GM does two moves:

* selects beliefs from other GMs that will compose its perception
* define predicates for its own beliefs (that will be infered from its perception)

Any GM that defines a belief predicate selected by the new GM is implicitly a child of the new GM. The GM's first move thus positions it
in the existing heterachy of GMs and Detectors (a Detector is a built-in GM that only believes its sensors and makes no predictions.)

A belief predicate from a Detector is a "level 1" belief. A belief predicate defined in a GM has a level 1 + the mininum level of its perceived beliefs.

Constraints on the selection of "perceived belief predicates" (perceptual set):

* No implied GM parent-child cycle is created
* Only one transitive path from a GM to any of its children
* The intersection of two GMs perceptual sets contains at most one belief predicate
* A level N + 2 belief predicate can not be selected if there is a level N belief predicate yet to be selected by a GM
* A perceptual set contains from 1 to 3 belief predicates

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

### Policy maker

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
