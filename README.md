# andy_karma
A cause-effect model generator for Andy's generative models.

*(UNDER CONSTRUCTION)*

`andy_karma` is an Elixir application that creates, and gives access to, dedicated `karma` pengines, each one capable of inducing a logic program that models action-to-belief cause and effect on behalf of its associated Andy generative model. 

## Karma

*Karma* is be a Prolog application exposed as a [SWI-Prolog](https://www.swi-prolog.org/) [pengine](https://pengines.swi-prolog.org/docs/index.html).

["Karma is the causality principle focusing on 1)causes, 2)actions, 3)effects, where it is the mind's phenomena that guide the actions that the actor performs. Buddhism trains the actor's actions for continued and uncontrived virtuous outcomes aimed at reducing suffering."](https://en.wikipedia.org/wiki/Causality#Buddhist_philosophy)

`Karma` implements the "evolving smarts" of a GM. It infers:
 * the GM's perceptions from predictions sent and prediction errors received
 * the GM's beliefs from its current perceptions
 * the prediction errors the GMs produces from received predictions and current beliefs
 * the predictions a GM makes from the latest beliefs and actions taken
 * actions to take to achieve a current goal belief or test a current opinion belief

`Karma` implements an adapted [Apperception Engine](https://arxiv.org/pdf/1910.02227.pdf) which, given past beliefs and actions, generates and updates a logic program (aka a `predictor`) that infers the actions a GM should take to alter or test its beliefs across rounds.

The predictor logic program embodies a cause-effect model. It is fit if, given past and current beliefs and actions, it

1. infers the correct actions to take in the current round to achieve target beliefs in the next round
2. retrodicts belief changes in past rounds given prior actions taken at the time

In other words, the predictor, given the beliefs at round N and the beliefs at round N + 1, infers what actions to take to achieve them. If the agent is currently at round N then the logic program infers what actions to take to achieve desired beliefs (goals to be achieved, opinions to be preserved). If the predictor is otherwise applied to a past round N, then it should correctly infer the known beliefs at round N + 1, given the beliefs and actions taken at round N.

Once the predictor's performance becomes unacceptable, it is re-generated.

### Generation

Karma is a Prolog application that induces a `predictor` for an associated Generative Model.

The input to Karma from Andy is a sequence of data from remembered rounds of the associated GM:
1. The beliefs held at the end of the round, as `belief(ConjectureName, AboutObject, Value, RoundIndex)` predicates
2. The actions taken at the end of the round, as `action(IntentName, ByObject, Value, Duration, RoundIndex)` predicates
3. The rounds remembered, as `round(RoundIndex, WhenStarted, WhenCompleted)`

Round data informs the search for a predictor (as cause-effect model) that satisfies the data.

The domain of action intent names is pre-defined and fixed (they represent what the agent can do), and so is the domain of associated action values and durations. The domain of belief conjecture names is an open-ended superset of those that seed the searched-for predictor program. In other words, Karma can invent conjecture names in its search for a predictor. Invented conjecture names (a, b, c etc.) might correspond to conditions such as "wheel slippage" or "unseen obstacle behind" etc. that are not in the GM's set of hard-coded conjectures.

Karma searches a constrained space of logic programs for one that fits the input data (it can predict and it can retrodict action-belief cause and effect). The search space is contrained by Immanuel Kant's rules of "synthetic unity of apperception", and by time spent and generated code complexity. The fitness function for the predictor is expressed in terms of the predictor's accuracy and complexity (the smaller the program, the better).

Unity of apperception constraints:

* Spatial: All 
* Conceptual
* Static
* Causal

* A predictor can never contain more than one belief predicate with the same conjecture name for a given round that's about the same individual.
* TBD

The best candidate predictor logic program is stored in the associated GM's pengine as a Prolog module named `predictor`.

### Legacy rules

A GM keeps only a certain number of past rounds in memory, with older ones eventually forgotten. These forgotten rounds are no longer available to participate in the generation of the predictor logic program, or in its validation. Important "rules" may have been learned in forgotten rounds because of possibly rare but crucial circumstances not encountered since. The logic program will conserve "rules" for as long as possible across versions of itself, even though they may no longer retrodict the retained experiences. 

A new version of a logic program will only shed "legacy rules" from prior versions if

1. they make it fail to predict/retrodict experiences from the retained rounds
2. they make the program exceed complexity limits
