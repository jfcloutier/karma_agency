# Beliefs

> Beliefs are meta-observations made by a Cognition Actor (CA) from examining its remembered states (a state is a set of observations of the CA's umwelt that are synchronous to one time frame).

There are three kinds of beliefs: **counts**, **trends** and **endings**.

The beliefs of a CA are exposed as properties to be observed by parent CAs.

A belief is about is a **synthetic object** named by the CA and whose nature (what is being counted, what is trending, what is ending) is private to the CA.

## Counts

> How many of something there are in the current state

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
