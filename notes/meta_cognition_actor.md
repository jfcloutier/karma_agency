# Meta-cognition actor

A meta-cognition actor (meta-CA) adds and removes cognition actors (CAs) at the level of the SOM that it manages.

A SOM expands from the bottom and shrinks from the top.

## Constraints

* There is one meta-CA per level L (sensor and effector CAs are at L=0, which is a prior level with no meta-CA)
* The (level) L meta-CA attempts to add one L CA per time frame until all L-1 CAs are covered (in some L CA's umwelt)
* When adding a level L CA,
  * The L meta-CA selects a random N >= 2 subset of all capable L-1 CAs (capable <=> with causal theory) as the new L CA's umwelt such that
    * it increases coverage of L-1 CAs
    * all effector CAs are transitively in the new CA's umwelt
  * A CA's umwelt is permament and static (it remains the same throughout the life of the CA)
* A meta-CA can not cull a relevant L CA (a CA is relevant if it is in the umwelt of an L+1 CA).

## Lifecycle

* The L meta-CA creates an L+1 meta-CA when Level L completely covers Level L-1
* A meta-CA removes itself from the SOM when there are no CAs at level L-1

## Continuity

* While fullness is critically low, a meta-CA removes, with each time frame, the most wasteful (cost/competence) of its non-relevant L CAs (if any)
  * Cost = local cost + shared cost of its transitive umwelt
    * if an L - 1 CA is in the umwelt of a level L CA and no others, then that CA inherits the entire (unshared) cost of the L - 1 CA
