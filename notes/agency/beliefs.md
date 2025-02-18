# Beliefs

> Beliefs are observations made by a Cognition Actor (CA) from examining its remembered states and from attempts at achieving goals.

A state is a set of observations of the CA's umwelt that are synchronous to one time frame.

There are five kinds of beliefs: **abductions**, **counts**, **trends**, **endings** and **attempts**.

The beliefs of a CA are exposed as properties to be observed by parent CAs.

A belief is synthesized by the CA and its nature (what is being imagined, what is being counted, what is trending, what is ending, what is attempted) is private to the CA.

## Abductions

> Observation expressed using a predicate and/or object imagined by the CA in order to formulate a causal theory of its direct observations.

Any observation abduced by the CA is elevated to the status of belief of the CA since it is never expressed in its umwelt.

* About
  * A relationship or property
* Value
  * An object (relationship) or domain value (property)
* Since when
  * Current time frame

## Counts

> How many of something there are in the current state, if there are more than one

e.g. self is next to 2 objects

* About
  * The number of relationships of a given type involving a given object
* Value
  * A non-zero integer
* Since when
  * Current time frame

Expressed as `count(Object, Value)` where

* `Object` is a unique name that the CA internally maps to
  * `relationships_counted(RelationshipName, to/from, ObservedObject)`

## Trends

> How something observed is changing across more than one of the latest states

e.g. going up a luminance gradient persists

* About
  * The values of properties of a given type of a given object
* Value
  * Stable, unstable, up, down
* Since when
  * Stable/Up/down - at least the last two frames
  * Unstable, otherwise since the end of the last not unsable trend

Expressed as `trend(Object, Value)` where

* `Object` is a unique name that the CA internally maps to
  * `property_trending(PropertyName, ObservedObject)`

## Endings

> How long before something observed in past states ends (most recent ending only)

e.g. the floor last stopped being green after 5 time frames

* About
  * Valued property of a given object
  * A given relation between two objects
* Value
  * Last duration in time frames
* Since when
  * Since ending

Expressed as `ending(Object, Value)` where

* `Object` is a unique name that the CA internally maps to
  * `property_ending(ObservedObject, Value)`
  * `relationship_ending(RelationshipName, ObservedObject1, ObservedObject2)`

## Attempts

> An attempt made at achieving a goal

e.g. attempted to stop the distance to a thing getting smaller

* About
  * A belief
* Value
  * validate, invalidate
* Since when
  * Current time frame

Expressed as `attempted(Belief, Value)` where

* `Belief` is any belief expression
  * `attempted(Belief, Value)`

## About naming synthetic beliefs

Names are generated in such as way as to be unique to the structure and contents of the belief, and determined by it such that if two CAs synthesize smeantically identical beliefs, they will be named identically without requiring concertation between CAs.
