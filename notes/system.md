# System

## Components

* karma system
  * karma_agent (prolog)
    * Apperception
    * Fitness
    * SOM
      * Cognition actors
      * Metacognition
    * body
      * interfaces with the karma_body app
  * karma_body (elixir/phoenix)
    * Web app with API
    * Defines the BodyParts behaviour
      * list actuators and sensors
      * sense and actuate on request
    * Propagate agent events as HTTP puts
      * Events about structure and behavior
    * Accesses an implementation of BodyParts (identified via configuration)
  * karma_lego_body lib (elixir)
    * Implements the BodyParts behaviour for the BrickPi3 agent
  * karma_simulated_body lib (elixir)
    * Implements the BodyParts behaviour for a simulated agent
  * karma_observer (elixir/liveview)
    * Web app with endpoints for receiving events
    * Accesses the body app of simulated or lego agent
    * Visualization of a simulated robot's location in 2-D space
    * Monitoring of an agent's
      * Homeostatic state
      * SOM state
      * Events

## Diagrams

```mermaid
---
title: System
---

graph LR;
agent("`**agent**
*prolog*`")==>|HTTP|body("`**body**
*phoenix*`") & observer("`**observer**
*liveview*`")
observer("`**observer**
*liveview*`")==>|HTTP|body("`**body**
*phoenix*`") 
```

----

``` mermaid
---
title: Behaviours
---

classDiagram
  Body <|-- LegoBody
  Body <|-- SimulatedBody
  class Body {
    +sensors()
    +actuators()
    +sense(sensor)
    +actuate(actuator, action)
  }
```

----

```mermaid
---
title: Interactions
---

sequenceDiagram;
    participant agent
    participant body
    participant observer
    agent-->>+body: ask for actuators/sensors
    body-->>-agent: answer actuator/sensors
    agent-->>+body: sense/actuate
    body-->>-agent: sensed/actuated
    observer-->>+body: ask for actuators/sensors
    body-->>-observer: answer actuator/sensors
    agent-->>observer: event
    agent-->>agent: event
```
