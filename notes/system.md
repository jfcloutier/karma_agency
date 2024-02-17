# System

## Notes

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

## Karma system

```mermaid
---
title: Networking
---

graph LR;
agency("`**agency**
*prolog on PC*`")==>|HTTP|body("`**body**
*elixir*`") & observer("`**observer**
*elixir*`")
observer("`**observer**
*elixir on PC*`")==>|HTTP|body("`**body**
*elixir on PC or robot*`") 
```

----

``` mermaid
---
title: System components
---

classDiagram
  Agency
  Observer
  Body <|-- LegoBody
  Body <|-- SimulatedBody
  Observer <--> Agency
  Observer --> Body
  Body <--> Agency
  class Agency {
    on PC
    Prolog app
    apperception()
    fitness()
    som()
    pubsub()
    body_interface()
  }
  class Observer {
    on PC
    Elixir web ppp
    monitor()
    virtual_world()
  }
  class Body {
    API
    sensors()
    actuators()
    sense(sensor)
    actuate(actuator, action)
  }
  class LegoBody {
    on BrickPi3 - Lego robot
    Elixir web app
  }
  class SimulatedBody {
    on PC
    Elixir web app
  }
```

----

```mermaid
---
title: System interactions
---

sequenceDiagram;
    participant agency
    participant body
    participant observer
    agency-->>+body: ask for actuators/sensors
    body-->>-agency: answer actuator/sensors
    agency-->>+body: sense/actuate
    body-->>-agency: sensed/actuated
    observer-->>+body: ask for actuators/sensors
    body-->>-observer: answer actuator/sensors
    agency-->>observer: SOM or fitness event
    agency-->>agency: SOM or fitness event
```

----
