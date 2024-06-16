# Meta-cognition actor

A meta-cognition actor adds and removes CAs at its level.

## Constraints

* Only competent (good predictor) CAs from level L - 1 can be in the (static) umwelt of a new CA at level L
* Level L is grown one CA at a time until all competent Level - 1 CAs are covered
* When creating a level L CA,
  * a meta-CA selects a random N >= 2 subset of all competent L-1 CAs as the new CA's umwelt such that
    * it increases coverage of Level - 1
    * all effector CAs are transitively in the umwelt.
* A meta-CA can not cull a relevant CA at its level L, i.e. if the CA is in the umwelt of a more abstract CA at level L + 1.
* A meta-CA culls a non-relevant level L CA if it is persistently incompetent.

## Lifecycle

* A level L + 1 meta-CA is started by the Level L meta-CA when Level L completely covers Level L - 1
* A meta-CA removes itself from the SOM when there are no CAs at level L - 1

## Continuity

* If fullness is critically low, the meta-CA removes the most unjustifiably costly (cost/competence) of its non-relevant ward CAs
  * Cost = local cost + shared cost of its transitive umwelt
    * if an L - 1 CA is in the umwelt of a level L CA and no others, then that CA inherits the entire (unshared) cost of the L - 1 CA
