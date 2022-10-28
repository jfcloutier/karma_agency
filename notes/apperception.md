# Karma's Apperception Engine

## Algorithm

Given an experience,
  Construct the apperception task
  Define a series of templates
    Given a time limit
      Take the next template
        Search for a working theory for the template
        Retain the highest rated theory found so far
      Repeat until time expired
  Return retained theory with its rating

## Constructing an apperception task

An apperception task consists of

Given an experience,
  Extract the sequence of sensory/action states
  Extract a minimal type signature from the sequence

### Minimal type signature

The smallest type signature TS satisfying the sequence.

It consists of

    * types T
        * value types
        * object types
    * typed objects O
    * typed predicates of unary and binary object properties P 
    * typed variables where each has an object or value type V

## Templates

A template is

* a possibly empty extension TS* to the apperception task's type signature
* plus restrictions on the size of a theory.

Trying successive templates leads to an iterative deepening search for theories.

### Template-generation algorithm

Define an infinite series of (T*, n) pairs from the cartesian product of

* An infinite series of T* (lists of types, the first one empty) each extending the task's types T
* And an infinite series of bounds n == (100, 200, 300...)
* Ordered ascending on size(T*) x n

For each (T*, n)
  Define a series of templates from the cartesian product from
    Types - {T + T*} - a singleton
    Os - list of lists of typed objects (given T + T*) - [Oi, Oi+1, ...], sorted ascending on size >= 0
    Ps - list of lists of typed predicates (given T + T*) - [Pi, Pi+1, ...] sorted ascending on size >= 0
    Vs - list of lists of typed variables (given T + T*) - [Vi, Vi+1, ...] sorted ascending on size >= 0
    Ns - Max numbers of static rules {0, 1, 2, ...}
    Nc - Max numbers of causal rules {0, 1, 2, ...}
    Na - Max numbers of atoms per rule
  Sort the series of templates fairly, smaller first
  Take the first n templates

Concatenate all series of templates in order

## Searching for theories

For each template,

* Generate all grounded and ungrounded terms implied by the template type signature T, O, P, V
* Use grounded and ungrounded terms to create a candidate theory:
  * abduce initial conditions
  * induce static rules, causal rules, and conceptual constraints (the spatial constraint is predefined)

* Compute a trace from the theory
* Verify that it satisfies the unity constraints, namely
  * spatial (pre-defined constraint)
  * conceptual (induced constraints)
  * (static and temporal unity are achieved by generating a trace)

* If verified,

  * evaluate the cost of the theory
    * size(theory) + weighted mis-coverage(trace, sequence)
  * replace the lowest cost one found if cheaper

### Abducing initial conditions

The initial conditions define state t=1 of the trace.

They are the first state of the task's sequence plus a non-redundant list of grounded atoms

### Inducing causal rules

Generate a list of Ns valid static rules from the unground terms.
Generate a list of Nc valid causal rules from the unground terms.
Subject to the max rule size constraint Na

### Inducing conceptual constraints

For every predicate p appearing the initial state or in a rule

* define a mutual exclusion constraint with one or more other predicates of the same type
* or, if binary on objects, define an existential rule

The spatial constraint is predefined as:

* Success if fail to find two objects o1 and o2 in any state of the trace such that o1 and o2 are not related directly or indirectly.

The static and causal constraints will be implicitly satisfied by the successful construction of a trace.

### Computing the trace

Starting with initial state as S1,

* While length(trace) < length(sequence) and no state is repeated
  * apply static rules to Si to validate and augment the state Si
  * carry over into what's not incomposible from Si-1
  * verify spatial and conceptual contraints
  * apply causal rules to Si to generate Si+1

## Costing a theory

A low-cost theory has low complexity and its trace covers well the apperception task's sequence.

If the len(trace) < length(sequence) then repeat the trace until it has the same length as the sequence.

To compute mis-coverage, for each SSi = state(sequence, i) and STi = state(trace, i), count the number of properties in SSi and not in STi.

To compute complexity, count the number of atoms in all the rules.

Cost = complexity + K * mis-coverage (K is TBD)

---------------------------------------------

## Encoding

### Sensed

sensed(Predicate, Objs, TimeIndex).

Example:

sensed(off, [a], 1).

### Task

task(Sequence, TypeSignature).

### Sequence

Ordered list of contiguous list (possibly empty) of simultaneous sensed

sequence(Observations, InitTime)
